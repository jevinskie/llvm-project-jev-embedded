add_llvm_pass_plugin(jev-embedded-customs
    EmbeddedCustoms.cpp
    DEPENDS
    intrinsics_gen
    SUBPROJECT
    jev-embedded-customs
)

# install(TARGETS jev-embedded-customs)

# install(TARGETS ${name} RUNTIME DESTINATION lib)
# add_llvm_install_targets(install-${name}
    # DEPENDS ${name}
    # COMPONENT ${name})
