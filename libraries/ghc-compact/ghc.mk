libraries/ghc-compact_PACKAGE = ghc-compact
libraries/ghc-compact_dist-install_GROUP = libraries
$(if $(filter ghc-compact,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/ghc-compact,dist-boot,0)))
$(if $(filter ghc-compact,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/ghc-compact,dist-install,1)))
$(if $(filter ghc-compact,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/ghc-compact,dist-install,2)))
