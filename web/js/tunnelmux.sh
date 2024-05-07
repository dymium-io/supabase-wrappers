#!/usr/bin/env bash

# Root of the git repository
repo_root=$(git rev-parse --show-toplevel)

# Start new session
tmux new-session -d -s tunnel

# Split the window into four panes
tmux split-window -h 
tmux split-window -v 
tmux select-pane -t 0
tmux split-window -v 

# Change directory in each pane
tmux send-keys -t tunnel:0.0 "cd $repo_root/Tunnels/go/server ; LOCAL_SEARCH= ./runping.sh" C-m
tmux send-keys -t tunnel:0.1 "cd $repo_root/Tunnels/go/client; " C-m
tmux send-keys -t tunnel:0.2 "cd $repo_root/Tunnels/go/server" C-m
tmux send-keys -t tunnel:0.3 "cd $repo_root/Tunnels/go/client" C-m

# Attach to the new session
tmux attach -t tunnel
