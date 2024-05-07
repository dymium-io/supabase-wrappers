#!/usr/bin/env bash

# Root of the git repository
repo_root=$(git rev-parse --show-toplevel)

# Start new session
tmux new-session -d -s portal

# Split the window into four panes
tmux split-window -h 
tmux split-window -v 
tmux select-pane -t 0
tmux split-window -v 

# Change directory in each pane
tmux send-keys -t portal:0.0 "cd $repo_root/web/go/assets ; LOCAL_SEARCH= ./run.sh" C-m
tmux send-keys -t portal:0.1 "cd $repo_root/web/js/packages/portal; yarn start" C-m
tmux send-keys -t portal:0.2 "cd $repo_root/web/js/packages/admin; yarn start" C-m
tmux send-keys -t portal:0.3 "cd $repo_root/web/go/src" C-m

# Attach to the new session
tmux attach -t portal
