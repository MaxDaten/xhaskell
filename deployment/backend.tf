terraform {
    backend "gcs" {
        # Managed by gitops
        bucket  = "ai-playground-c437-terraform-state"
        prefix  = "xhaskell"
    }
}
