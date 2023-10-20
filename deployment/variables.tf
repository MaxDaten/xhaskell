
variable "project" {
  type        = string
  description = "name of the project for example: myproject"
}

variable "location" {
  type        = string
  description = "location of the project for example: us-east-1"
}

variable "artifact-registry" {
  type        = string
  description = "full qualified name of the artifact registry for example: us-east-1-docker.pkg.dev/myproject/docker"
}

variable "image-name" {
  type        = string
  description = "name of the image for example: myimage"
}

variable "image-tag" {
  type        = string
  description = "tag of the image for example: latest"
}
