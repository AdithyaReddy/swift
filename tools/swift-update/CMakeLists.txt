add_swift_host_tool(swift-update
  swift-update.cpp
  LINK_LIBRARIES
    swiftDriver
    swiftFrontend
    swiftSema
    swiftSyntax
  SWIFT_COMPONENT
    tools
  COMPONENT_DEPENDS
    Support
)

