HOME=/home/scott
PROGPATH=$HOME/programs/src
PERMISSIONSPATH=$PROGPATH/onping-permissions
CORETYPESPATH=$PERMISSIONSPATH/core-types
CORELIBSPATH=$PERMISSIONSPATH/core-libs
OPTSPATH=$PERMISSIONSPATH/opts-libs
APPPATH=$PERMISSIONSPATH/apps
cabal sandbox add-source $CORELIBSPATH/onping-permissions-core
cabal sandbox add-source $CORETYPESPATH/onping-permission-types
cabal sandbox add-source $OPTSPATH/onping-permissions-graphviz
cabal sandbox add-source $PROGPATH/persist-mongo-template
cabal sandbox add-source $PROGPATH/structured-script
