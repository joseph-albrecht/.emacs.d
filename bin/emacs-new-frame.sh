#!/usr/bin/env bash
# Get list of all Emacs windows before the new frame
existing_windows=$(/usr/bin/env osascript -e 'tell application "Emacs" to return name of every window')

# Create the new frame
emacsclient -e '(make-frame-command)'
emacsclient -e '(message "hello from better touch tools")'

# Small sleep to ensure the new frame has time to be created
# sleep 0.5

# Find the new window
new_window=$(/usr/bin/env osascript -e 'tell application "Emacs" to return name of every window' | grep -v "$existing_windows")

# If found, bring it to front
if [[ ! -z "$new_window" ]]; then
    /usr/bin/env osascript -e "tell application \"Emacs\" to set index of window \"$new_window\" to 1"
fi

