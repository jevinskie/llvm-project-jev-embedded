add_subdirectory(3rdparty)

add_llvm_pass_plugin(jev-embedded-customs
    EmbeddedCustoms.cpp
    DEPENDS
    intrinsics_gen
    fmt
    SUBPROJECT
    jev-embedded-customs
)

target_link_libraries(jev-embedded-customs PRIVATE fmt)

# install(TARGETS jev-embedded-customs)

# install(TARGETS ${name} RUNTIME DESTINATION lib)
# add_llvm_install_targets(install-${name}
    # DEPENDS ${name}
    # COMPONENT ${name})
