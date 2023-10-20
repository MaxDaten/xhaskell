
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
  depends_on = [google_cloud_run_v2_service_iam_member.github-actions-sa]
}

# data of current used service account
data "google_service_account" "github-actions-sa" {
  account_id = "github-actions"
}

# Allow github actions to deploy to google cloud run
resource "google_cloud_run_v2_service_iam_member" "github-actions-sa" {
  project  = var.project_id
  location = var.location
  name     = var.image-name
  role     = "roles/run.admin"
  member   = data.google_service_account.github-actions-sa.member
}
