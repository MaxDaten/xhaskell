
resource "google_cloud_run_v2_service" "default" {
  name     = var.image-name
  location = var.location
  ingress  = "INGRESS_TRAFFIC_ALL"

  template {
    timeout                          = "300s"
    max_instance_request_concurrency = 80
    execution_environment            = "EXECUTION_ENVIRONMENT_GEN1"

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
