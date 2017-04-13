add_swift_host_tool(swift-syntax-tblgen
  swift-syntax-tblgen.cpp
  LLVM_COMPONENT_DEPENDS
    Support
    TableGen
  SWIFT_COMPONENT tools
)
add_dependencies(swift-syntax-all-generated-source swift-syntax-tblgen)

