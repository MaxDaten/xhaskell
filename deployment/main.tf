
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
        cpu_idle          = false
        startup_cpu_boost = true
      }
    }

    scaling {
      min_instance_count = 0
      max_instance_count = 2
    }
  }
}

# Service Account for this Service

resource "google_service_account" "default" {
  account_id   = "service-account-${var.image-name}"
  display_name = "Service Account for ${var.image-name}"
}
