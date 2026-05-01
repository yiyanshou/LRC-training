# Builds shinylive payload
shinylive::export("compare_LRC",
                  "docs",
                  template_params = list(title = "LRC Comparison"))

# Run app locally
httpuv::runStaticServer("docs")
