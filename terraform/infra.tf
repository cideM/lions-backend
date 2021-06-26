# https://www.digitalocean.com/community/tutorials/how-to-use-terraform-with-digitalocean
variable "do_token" {}
variable "pvt_key" {}

terraform {
  backend "s3" {
    bucket = "lions-achern-terraform-remote-state-storage-s3"
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

provider "aws" {
  region = "eu-central-1"
}

provider "digitalocean" {
  token = var.do_token
}

data "digitalocean_ssh_key" "terraform" {
  name = "FB desktop"
}

resource "aws_route53_zone" "lions-primary" {
  name = "lions-achern.de"
}

resource "aws_route53_record" "cname-netlify" {
  zone_id = aws_route53_zone.lions-primary.zone_id
  name    = "www.lions-achern.de"
  type    = "CNAME"
  ttl     = "300"

  records = [
    "lions-achern.netlify.com.",
  ]
}

resource "aws_route53_record" "main-soa" {
  zone_id = aws_route53_zone.lions-primary.zone_id
  name    = "lions-achern.de"
  type    = "SOA"
  ttl     = "300"

  records = [
    "dns1.p05.nsone.net. hostmaster.nsone.net. 1 7200 900 1209600 86400",
  ]
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
}

resource "aws_route53_record" "amazonses_verification" {
  zone_id = aws_route53_zone.lions-primary.zone_id
  name    = "_amazonses.lions-achern.de"
  type    = "TXT"
  ttl     = "600"
  records = [aws_ses_domain_identity.lions.verification_token]
}

resource "aws_ses_domain_dkim" "lions" {
  domain = aws_ses_domain_identity.lions.domain
}

# Litestream SQLite replication
# This only let's "aws ls" use the specified buckets! Running just "aws s3 ls"
# without specifying the bucket name will result in "Access Denied".
# $ terraform output -json
# $ AWS_ACCESS_KEY_ID= AWS_SECRET_ACCESS_KEY= aws s3 ls lions-achern-litestream-replica-1
module "s3_user" {
  source = "cloudposse/iam-s3-user/aws"
  version     = "0.15.2"
  name         = "litestream_user"
  s3_actions   = ["s3:*"]
  s3_resources = [
    "${aws_s3_bucket.litestream-replica-1.arn}/*",
    "${aws_s3_bucket.litestream-replica-1.arn}",
    "${aws_s3_bucket.event-attachments.arn}"
   ]
}

output "litestream_user_access_key_id" {
  value = module.s3_user.access_key_id
  sensitive = true
}

output "litestream_user_access_key_secret" {
  value = module.s3_user.secret_access_key
  sensitive = true
}

resource "aws_s3_bucket" "event-attachments" {
    bucket = "lions-achern-event-attachments"

    versioning {
      enabled = true
    }

    lifecycle {
      prevent_destroy = true
    }
}

resource "aws_s3_bucket" "litestream-replica-1" {
    bucket = "lions-achern-litestream-replica-1"

    versioning {
      enabled = true
    }

    lifecycle {
      prevent_destroy = true
    }
}

resource "aws_s3_bucket" "terraform-state-storage-s3" {
    bucket = "lions-achern-terraform-remote-state-storage-s3"

    versioning {
      enabled = true
    }

    lifecycle {
      prevent_destroy = true
    }
}

resource "aws_route53_record" "_amazonses_dkim_record" { 
  count   = 3
  zone_id = aws_route53_zone.lions-primary.zone_id
  name    = "${element(aws_ses_domain_dkim.lions.dkim_tokens, count.index)}._domainkey.lions-achern.de"
  type    = "CNAME"
  ttl     = "600"
  records = ["${element(aws_ses_domain_dkim.lions.dkim_tokens, count.index)}.dkim.amazonses.com"]
}

resource "aws_ses_domain_identity" "lions" {
  domain = "lions-achern.de"
}

data "aws_caller_identity" "current" {}

resource "aws_cloudtrail" "cloudtrail" {
  name                          = "cloudtrail"
  s3_bucket_name                = aws_s3_bucket.log_bucket.id
  include_global_service_events = true
  is_organization_trail         = false
  depends_on                    = [aws_s3_bucket.log_bucket]
}

resource "aws_iam_user" "email" {
  name = "EmailFirebase"
}

resource "aws_iam_user_policy_attachment" "email_attachment" {
  user = aws_iam_user.email.name
  policy_arn = "arn:aws:iam::aws:policy/AmazonSESFullAccess"
}

resource "aws_iam_access_key" "email-user" {
  user    = aws_iam_user.email.name
}

output "email_user_key" {
  value = aws_iam_access_key.email-user.id
  sensitive = true
}

output "email_user_secret" {
  value = aws_iam_access_key.email-user.secret
  sensitive = true
}

resource "aws_iam_user" "admin" {
  name = "Admin"
}

resource "aws_iam_user_policy_attachment" "admin_attachment" {
  user = aws_iam_user.admin.name
  policy_arn = "arn:aws:iam::aws:policy/AdministratorAccess"
}

resource "aws_s3_bucket" "log_bucket" {
  bucket = "lions-achern-log-bucket"

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
            "Resource": "arn:aws:s3:::lions-achern-log-bucket"
        },
        {
            "Sid": "AWSCloudTrailWrite20150319",
            "Effect": "Allow",
            "Principal": {
              "Service": "cloudtrail.amazonaws.com"
            },
            "Action": "s3:PutObject",
            "Resource": "arn:aws:s3:::lions-achern-log-bucket/AWSLogs/${data.aws_caller_identity.current.account_id}/*",
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
            "Resource": "arn:aws:s3:::lions-achern-log-bucket/AWSLogs/o-z1m4oiv8ia/*",
            "Condition": {
                "StringEquals": {
                    "s3:x-amz-acl": "bucket-owner-full-control"
                }
            }
        }
    ]
}
POLICY
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
