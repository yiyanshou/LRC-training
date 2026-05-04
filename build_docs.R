# Builds shinylive payload
shinylive::export("compare_LRC",
                  "docs",
                  template_params = list(title = "LRC Comparison"))

# Generate manifest for Posit Connect
rsconnect::writeManifest("compare_LRC")

# Run WASM app locally
httpuv::runStaticServer("docs")
