include(TableGen)

# This needs to be a macro since tablegen (which is a function) needs to set
# variables in its parent scope.
macro(swift_tablegen)
  tablegen(SWIFT ${ARGN})
endmacro()

# This needs to be a macro since add_public_tablegen_target (which is a
# function) needs to set variables in its parent scope.
macro(swift_add_public_tablegen_target target)
  add_public_tablegen_target(${target})
endmacro()

# This needs to be a macro since add_public_tablegen_target (which is a
# function) needs to set variables in its parent scope.
macro(swift_add_tablegen target td_file ${SOURCES})
  set(LLVM_TARGET_DEFINITIONS td_file)
  swift_tablegen(td_file)
  add_tablegen(${target} SWIFT ${SOURCES})
endmacro()
