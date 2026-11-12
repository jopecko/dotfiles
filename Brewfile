# Shared Homebrew packages. Install with `make brew`.
# Work and per-machine packages go in ~/.Brewfile.local, which is not in the repo.

tap "agavra/tap", trusted: { formulae: ["tuicr"] }
tap "argoproj/tap", trusted: true
tap "coursier/formulas", trusted: true
tap "filosottile/musl-cross", "https://github.com/FiloSottile/homebrew-musl-cross"
tap "getsentry/tools"
tap "homebrew/core"
tap "peonping/tap", "https://github.com/PeonPing/homebrew-tap"
# Get stuff done with container-native workflows for Kubernetes
brew "argo"
# GitOps Continuous Delivery for Kubernetes
brew "argocd"
# Official Amazon AWS command-line interface
brew "awscli"
# Bourne-Again SHell, a UNIX command interpreter
brew "bash"
# Programmable completion for Bash 4.2+
brew "bash-completion@2"
# Clone of cat(1) with syntax highlighting and Git integration
brew "bat"
# Bash Automated Testing System
brew "bats-core"
# Resource monitor. C++ version and continuation of bashtop and bpytop
brew "btop"
# Container runtimes on MacOS (and Linux) with minimal setup
brew "colima", restart_service: :changed
# GNU File, Shell, and Text utilities
brew "coreutils"
# Pure Scala Artifact Fetching
brew "coursier"
# Tool for interacting with remote images and registries
brew "crane"
# Load/unload environment variables based on $PWD
brew "direnv"
# Tool for exploring each layer in a docker image
brew "dive"
# Pack, ship and run any application as a lightweight container
brew "docker"
# Docker CLI plugin for extended build capabilities with BuildKit
brew "docker-buildx"
# Isolated development environments using Docker
brew "docker-compose"
# Platform keystore credential helper for Docker
brew "docker-credential-helper"
# Docker Credential Helper for Amazon ECR
brew "docker-credential-helper-ecr"
# Modern, maintained replacement for ls
brew "eza"
# Open-source framework for augmenting humans using AI
brew "fabric-ai"
# Simple, fast and user-friendly alternative to find
brew "fd"
# Collection of GNU find, xargs, and locate
brew "findutils"
# Command-line fuzzy finder written in Go
brew "fzf"
# GitHub command-line tool
brew "gh"
# Distributed revision control system
brew "git"
# Syntax-highlighting pager for git and diff output
brew "git-delta"
# Quickly rewrite git repository history
brew "git-filter-repo"
# Git extension for versioning large files
brew "git-lfs"
# GNU implementation of the famous stream editor
brew "gnu-sed"
# Open source programming language to build simple/reliable/efficient software
brew "go"
# Task is a task runner/build tool that aims to be simpler and easier to use
brew "go-task"
# Kubernetes package manager
brew "helm"
# Feature-rich interactive Jira CLI
brew "jira-cli"
# Lightweight and flexible command-line JSON processor
brew "jq"
# Little helper to run CNCF's k3s in Docker
brew "k3d"
# Kubernetes CLI To Manage Your Clusters In Style!
brew "k9s"
# Generic command-line non-JVM Apache Kafka producer and consumer
brew "kcat"
# Colorize your kubectl output
brew "kubecolor"
# Kubernetes command-line interface
brew "kubernetes-cli"
# Template-free customization of Kubernetes YAML manifests
brew "kustomize"
# Simple terminal UI for git commands
brew "lazygit"
# Postgres C API library
brew "libpq"
# Run LogQL queries against a Loki server
brew "logcli"
# Ambitious Vim-fork focused on extensibility and agility
brew "neovim"
# Port scanning utility for large networks
brew "nmap"
# Node.js virtual environment builder
brew "nodeenv"
# Manage multiple Node.js versions
brew "nvm"
# Create, run, and share large language models (LLMs)
brew "ollama", restart_service: :changed
# Drop-in replacement for Terraform. Infrastructure as Code Tool
brew "opentofu"
# Interpreted, interactive, object-oriented programming language
brew "python@3.14"
# CLI for Postgres with auto-completion and syntax highlighting
brew "pgcli"
# Execute binaries from Python packages in isolated environments
brew "pipx"
# Python version management
brew "pyenv"
# Pyenv plugin to manage virtualenv
brew "pyenv-virtualenv"
# Persistent key-value database, with built-in net interface
brew "redis"
# Search tool like grep and The Silver Searcher
brew "ripgrep"
# Rust toolchain installer
brew "rustup"
# Scala language runner and build tool
brew "scala-cli"
# Static analysis and lint tool, for (ba)sh scripts
brew "shellcheck"
# Autoformat shell script source code
brew "shfmt"
# Tail multiple Kubernetes pods & their containers
brew "stern"
# Organize software neatly under a single directory tree (e.g. /usr/local)
brew "stow"
# Terminal multiplexer
brew "tmux"
# Code review TUI with vim keybindings
brew "tuicr"
# Extremely fast Python package installer and resolver, written in Rust
brew "uv"
# Executes a program periodically, showing output fullscreen
brew "watch"
# Internet file retriever
brew "wget"
# CLI for Git worktree management, designed for parallel AI agent workflows
brew "worktrunk"
# Malware identification and classification tool
brew "yara"
# JavaScript package manager
brew "yarn"
# Process YAML, JSON, XML, CSV and properties documents from the CLI
brew "yq"
# Feature-rich command-line audio/video downloader
brew "yt-dlp"
# Linux cross compilers based on musl libc
brew "filosottile/musl-cross/musl-cross", trusted: true
# Sentry command-line client for some generic tasks
brew "getsentry/tools/sentry-cli", trusted: true
# Sound effects and desktop notifications for AI coding agents
brew "peonping/tap/peon-ping", trusted: true
vscode "scala-lang.scala"
vscode "scalameta.metals"
krew "krew"
krew "neat"
krew "tree"
npm "corepack"

if OS.mac?
  tap "docker/tap", trusted: { casks: ["sbx@nightly"] }
  tap "felixkratz/formulae", "https://github.com/FelixKratz/homebrew-formulae", trusted: true
  tap "nikitabobko/tap"
  tap "steipete/tap"
  # A window border system for macOS
  brew "felixkratz/formulae/borders"
  # Custom macOS statusbar with shell plugin, interaction and graph support
  brew "felixkratz/formulae/sketchybar"
  # AeroSpace is an i3-like tiling window manager for macOS
  cask "nikitabobko/tap/aerospace", trusted: true
  # OpenAI's coding agent that runs in your terminal
  cask "codex"
  # Menu bar usage monitor for Codex and Claude
  cask "steipete/tap/codexbar", trusted: true
  cask "font-fira-code-nerd-font"
  cask "font-hack-nerd-font"
  cask "font-jetbrains-mono-nerd-font"
  # GPU-based terminal emulator
  cask "kitty"
  # Build, run, and govern agents across the software development lifecycle
  cask "docker/tap/sbx", trusted: true
  # Open-source code editor
  cask "visual-studio-code"
end

local_brewfile = File.expand_path("~/.Brewfile.local")
instance_eval(File.read(local_brewfile), local_brewfile) if File.exist?(local_brewfile)
