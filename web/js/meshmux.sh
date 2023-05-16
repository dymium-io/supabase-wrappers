#!/bin/bash

# Root of the git repository
repo_root=$(git rev-parse --show-toplevel)

# Start new session
tmux new-session -d -s mesh

# Split the window into four panes
tmux split-window -h 
tmux split-window -v 
tmux select-pane -t 0
tmux split-window -v 

# Change directory in each pane
tmux send-keys -t mesh:0.0 "cd $repo_root/Tunnels/go/meshserver ; ./run.sh" C-m
tmux send-keys -t mesh:0.1 "cd $repo_root/Tunnels/go/meshconnector; ./run.sh" C-m
tmux send-keys -t mesh:0.2 "cd $repo_root/Tunnels/go/meshserver" C-m
tmux send-keys -t mesh:0.3 "cd $repo_root/Tunnels/go/meshconnector" C-m

# Attach to the new session
tmux attach -t mesh
