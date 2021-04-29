# https://www.digitalocean.com/community/tutorials/how-to-use-terraform-with-digitalocean
variable "do_token" {}
variable "pvt_key" {}

terraform {
  backend "s3" {
    bucket = "lions-terraform-state"
    key    = "tfstate"
    region = "eu-central-1"
  }

  required_providers {
    digitalocean = {
      source  = "digitalocean/digitalocean"
      version = "1.22.2"
    }

    aws = {
      source  = "hashicorp/aws"
      version = "~> 2.70"
    }
  }
}

#########################
#      PROVIDERS        #
#########################
provider "aws" {
  region = "eu-central-1"
}

provider "aws" {
  region = "eu-central-1"

  # Not actual root user but IAM user in root account
  alias = "root"
}

provider "aws" {
  region = "eu-central-1"

  assume_role {
    role_arn = local.role_arns.shared
  }

  alias = "shared"
}

provider "digitalocean" {
  token = var.do_token
}

data "digitalocean_ssh_key" "terraform" {
  name = "FB desktop"
}

#########################
#      SHARED           #
#########################

resource "aws_route53_zone" "lions-primary" {
  name = "lions-achern.de"

  provider = aws.shared
}

resource "aws_ecr_repository" "download-public-keys" {
  name                 = "download-public-keys"
  image_tag_mutability = "MUTABLE"
  image_scanning_configuration {
    scan_on_push = true
  }

  provider = aws.shared
}

resource "aws_route53_record" "cname-netlify" {
  zone_id = aws_route53_zone.lions-primary.zone_id
  name    = "www.lions-achern.de"
  type    = "CNAME"
  ttl     = "300"

  records = [
    "lions-achern.netlify.com.",
  ]

  provider = aws.shared
}

resource "aws_route53_record" "main-soa" {
  zone_id = aws_route53_zone.lions-primary.zone_id
  name    = "lions-achern.de"
  type    = "SOA"
  ttl     = "300"

  records = [
    "dns1.p05.nsone.net. hostmaster.nsone.net. 1 7200 900 1209600 86400",
  ]

  provider = aws.shared
}

resource "aws_route53_record" "main-ns" {
  zone_id = aws_route53_zone.lions-primary.zone_id
  name    = "lions-achern.de"
  type    = "NS"
  ttl     = "300"

  records = [
    "dns1.p05.nsone.net.",
    "dns2.p05.nsone.net.",
    "dns3.p05.nsone.net.",
    "dns4.p05.nsone.net.",
  ]

  provider = aws.shared
}

resource "aws_route53_record" "amazonses_verification" {
  zone_id = aws_route53_zone.lions-primary.zone_id
  name    = "_amazonses.lions-achern.de"
  type    = "TXT"
  ttl     = "600"
  records = [aws_ses_domain_identity.lions.verification_token]

  provider = aws.shared
}

resource "aws_ses_domain_dkim" "lions" {
  domain = aws_ses_domain_identity.lions.domain

  provider = aws.shared 
} 

resource "aws_route53_record" "_amazonses_dkim_record" { 
  count   = 3
  zone_id = aws_route53_zone.lions-primary.zone_id
  name    = "${element(aws_ses_domain_dkim.lions.dkim_tokens, count.index)}._domainkey.lions-achern.de"
  type    = "CNAME"
  ttl     = "600"
  records = ["${element(aws_ses_domain_dkim.lions.dkim_tokens, count.index)}.dkim.amazonses.com"]

  provider = aws.shared
}

resource "aws_ses_domain_identity" "lions" {
  domain = "lions-achern.de"

  provider = aws.shared
}

#########################
#      ROOT STUFF       #
#########################

resource "aws_organizations_organization" "lions" {
  aws_service_access_principals = [
    "cloudtrail.amazonaws.com",
    "config.amazonaws.com",
  ]
  feature_set = "ALL"

  provider = aws.root
}

data "aws_caller_identity" "current" {}

resource "aws_cloudtrail" "lionstrail" {
  name                          = "lions-cloudtrail"
  s3_bucket_name                = aws_s3_bucket.log_bucket.id
  include_global_service_events = true
  is_organization_trail         = true
  depends_on                    = [aws_s3_bucket.log_bucket]

  provider = aws.root
}

resource "aws_s3_bucket" "log_bucket" {
  bucket = "lions-log-bucket"

  policy = <<POLICY
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "AWSCloudTrailAclCheck20150319",
            "Effect": "Allow",
            "Principal": {
              "Service": "cloudtrail.amazonaws.com"
            },
            "Action": "s3:GetBucketAcl",
            "Resource": "arn:aws:s3:::lions-log-bucket"
        },
        {
            "Sid": "AWSCloudTrailWrite20150319",
            "Effect": "Allow",
            "Principal": {
              "Service": "cloudtrail.amazonaws.com"
            },
            "Action": "s3:PutObject",
            "Resource": "arn:aws:s3:::lions-log-bucket/AWSLogs/${data.aws_caller_identity.current.account_id}/*",
            "Condition": {
                "StringEquals": {
                    "s3:x-amz-acl": "bucket-owner-full-control"
                }
            }
        },
        {
            "Sid": "AWSCloudTrailWrite20150319",
            "Effect": "Allow",
            "Principal": {
                "Service": [
                    "cloudtrail.amazonaws.com"
                ]
            },
            "Action": "s3:PutObject",
            "Resource": "arn:aws:s3:::lions-log-bucket/AWSLogs/o-z1m4oiv8ia/*",
            "Condition": {
                "StringEquals": {
                    "s3:x-amz-acl": "bucket-owner-full-control"
                }
            }
        }
    ]
}
POLICY

  provider = aws.root
}


resource "aws_organizations_account" "shared" {
  name      = "shared"
  email     = "lions-achern-aws+shared@protonmail.com"
  role_name = "OrganizationAccountAccessRole"
  # There is no AWS Organizations API for reading role_name
  lifecycle {
    ignore_changes = [
      role_name
    ]
  }
  tags = { Name = "Shared" }

  provider = aws.root
}

resource "aws_iam_user" "admin" {
  name = "Administrator"
  path = "/"

  provider = aws.root
}

resource "aws_iam_group" "administrators" {
  name = "Administrators"
  path = "/"

  provider = aws.root
}

# AWS Managed Policy
data "aws_iam_policy" "AdministratorAccess" {
  arn = "arn:aws:iam::aws:policy/AdministratorAccess"

  provider = aws.root
}

resource "aws_iam_group_policy_attachment" "AdministratorAccess" {
  group      = aws_iam_group.administrators.name
  policy_arn = data.aws_iam_policy.AdministratorAccess.arn

  provider = aws.root
}

#########################
#      DIGITAL OCEAN    #
#########################

# I created this manually because it uses a NixOS image build with Nix and
# uploaded manually

# resource "digitalocean_droplet" "lions-api" {
#   name               = "lions-api"
#   region             = "fra1"
#   size               = "s-1vcpu-1gb"
#   private_networking = true
#   ssh_keys = [
#     data.digitalocean_ssh_key.terraform.id
#   ]
#   connection {
#     host        = self.ipv4_address
#     user        = "root"
#     type        = "ssh"
#     private_key = file(var.pvt_key)
#     timeout     = "2m"
#   }
# }

# output "droplet-ip" {
#   value = digitalocean_droplet.lions-api.ipv4_address
# }
