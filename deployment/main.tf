
resource "google_cloud_run_v2_service" "default" {
  name     = var.image-name
  location = var.location
  ingress  = "INGRESS_TRAFFIC_ALL"

  template {
    timeout                          = "300s"
    max_instance_request_concurrency = 80
    execution_environment            = "EXECUTION_ENVIRONMENT_GEN1"
    service_account                  = google_service_account.default.email

    containers {
      image = "${var.artifact-registry}/${var.image-name}:${var.image-tag}"

      resources {
        limits = {
          cpu    = "1000m"
          memory = "128Mi"
        }
        cpu_idle          = true
        startup_cpu_boost = true
      }
    }

    scaling {
      min_instance_count = 0
      max_instance_count = 2
    }
  }
}

data "google_service_account" "github-actions-sa" {
  account_id = "github-actions"
}

# Service Account for this Service

resource "google_service_account" "default" {
  account_id   = "${var.image-name}-sa"
  display_name = "Service Account for ${var.image-name}"
}


# actAs for github-actions-sa

resource "google_service_account_iam_member" "github-actions-sa-actAs" {
  service_account_id = google_service_account.default.name
  role               = "roles/iam.serviceAccountTokenCreator"
  member             = data.google_service_account.github-actions-sa.member
}
