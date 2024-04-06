#!/bin/bash

LAYOUT_FILE=~/.functions/layout.txt

# Set layout according to local and global settings
function run_layout {
    # Check if layout is set locally
    if [[ ! -z $LAYOUT ]]
        then
            echo "Local layout set to $LAYOUT"
            return 0
    fi

    # Check if layout is set globally
    if [[ ! -f $LAYOUT_FILE ]]
        then
            echo "file does not exist"
            return 1
        else if [[ ! -s $LAYOUT_FILE ]]
            then
                echo "file is empty"
                return 0
            else
                CONTENTS="$(cat $LAYOUT_FILE)"

                if [[ $CONTENTS = C* || $CONTENTS = c* ]]
                    then
                        echo "Set to ColemakDH"
                        LAYOUT=ColemakDH
                        return 0
                fi
                if [[ $CONTENTS == Q* || $CONTENTS == q* ]]
                    then
                        echo "Set to Qwerty"
                        export LAYOUT=Qwerty
                        return 0
                fi
        fi
    fi
}

# Set global layout
function set_global_layout {
    echo $1 > $LAYOUT_FILE
    echo "Set global layout to $1"
    run_layout
}
