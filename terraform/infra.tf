terraform {
  backend "s3" {
    bucket = "lions-achern-terraform-remote-state-storage-s3"
    key    = "tfstate"
    region = "eu-central-1"
  }

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.17.0"
    }
  }
}

provider "aws" {
  region = "eu-central-1"
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
  version     = "1.2.0"
  name         = "litestream_user"
  s3_actions   = ["s3:*"]
  s3_resources = [
    "${aws_s3_bucket.litestream-replica-1.arn}/*",
    "${aws_s3_bucket.litestream-replica-1.arn}",
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

resource "aws_s3_bucket" "litestream-replica-1" {
    bucket = "lions-achern-litestream-replica-1"
    lifecycle {
      prevent_destroy = true
    }
}

resource "aws_s3_bucket" "terraform-state-storage-s3" {
    bucket = "lions-achern-terraform-remote-state-storage-s3"

    lifecycle {
      prevent_destroy = true
    }
}

resource "aws_s3_bucket_versioning" "litestream-versioning" {
  bucket = aws_s3_bucket.litestream-replica-1.id
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_versioning" "tfstate-versioning" {
  bucket = aws_s3_bucket.terraform-state-storage-s3.id
  versioning_configuration {
    status = "Enabled"
  }
}

resource "aws_s3_bucket_lifecycle_configuration" "versioning-bucket-config" {
  bucket = aws_s3_bucket.litestream-replica-1.id
  depends_on = [aws_s3_bucket_versioning.litestream-versioning]

  rule {
    id = "config"

    filter {
      prefix = "fly-members-1/"
    }

    noncurrent_version_expiration {
      newer_noncurrent_versions = 5
      noncurrent_days = 5
    }

    status = "Enabled"
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

resource "aws_s3_bucket_policy" "logs" {
  bucket = aws_s3_bucket.log_bucket.id
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

resource "aws_s3_bucket" "log_bucket" {
  bucket = "lions-achern-log-bucket"
}
