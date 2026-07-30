# Git Workflow Guidance

Canonical git, branch, worktree, review, and merge workflow guidance for this
repository.

## User Control

Never perform state-changing git operations without an explicit user request.
This includes:

- `git add`, `git commit`, `git commit --amend`
- `git push`
- `git merge`, `git merge --squash`, `git rebase`
- `git reset --hard` or `git reset --mixed`
- deleting branches or removing worktrees
- creating pull requests through any tool

Safe read-only operations are allowed: `git status`, `git diff`, `git log`,
`git branch` as a list operation, and `git worktree list`.

### Human-Owned Commit and Push Workflow

The human performs commits and pushes by default. An agent's normal workflow
ends after editing and validation, with changes left uncommitted and unpushed.
Do not propose, initiate, or treat committing or pushing as a routine completion
step.

An agent may perform a commit or push only as a rare exception using this
two-step authorization process:

1. The human directly asks the agent to perform that specific Git operation.
2. Immediately before acting, the agent restates the exact operation and asks
   the human to confirm it.
3. The agent runs the operation only after receiving fresh, unambiguous
   confirmation.

Before confirming a commit, report the exact files or staged scope, validation
results, and proposed commit message. Before confirming a push, report the
branch, exact commit SHA or SHAs, remote, and destination ref.

Commit and push authorization are separate. A request or confirmation to commit
never authorizes a push. Each direct request and confirmation applies only to
the stated operation and expires after it is performed. Earlier approval, an
existing remote branch, or an existing pull request never authorizes another
commit or push.

Requests to implement, fix, build, validate, finish a plan, publish, prepare a
pull request, update a pull request, or continue work are not direct requests to
commit or push. If the human has not directly requested the agent to perform the
operation, leave changes uncommitted or commits unpushed for the human to handle.

When a task requires renaming or moving a tracked file, always use
`git mv <old_path> <new_path>` instead of a filesystem rename, copy, or
delete/recreate sequence so Git records the change as a move and preserves file
history. Treat `git mv` as a state-changing operation: use it only when the user
has requested or approved the rename or move.

## Branch Strategy

Use `main` as the stable integration branch. New feature branches and worktrees
should branch from `main`, not from another feature branch.

## Worktree Workflow

Use worktrees when a long analysis is running in the main checkout or when a
large change should be isolated.

```powershell
# 1. Start from main and update it.
git checkout main
git pull origin main

# 2. Create the worktree. The -b flag comes before the path.
git worktree add -b <branch_name> ..\PastHumanImpact_<feature_name>

# 3. Verify and open.
git worktree list
code -n ..\PastHumanImpact_<feature_name>
```

In the new worktree, restore the R environment before running analyses:

```r
renv::restore(lockfile = here::here("renv/library_list.lock"))
```

Data are not bundled with the repository. Each worktree needs access to the
same external `Data/` structure described in `README.md` and configured via
`secrets.yaml` or `R/00_Config_file.R`.

## Targets Stores

PastHumanImpact writes target stores under the external data storage path:

- `Targets_data/pipeline_pollen_data`
- `Targets_data/pipeline_paps`
- `Targets_data/pipeline_events`
- `Targets_data/pipeline_predictors`
- `Targets_data/analyses_h1`
- `Targets_data/analyses_h2`

When copying target stores between worktrees, copy only the specific store that
was intentionally regenerated. Do not bulk-copy the entire data directory.

## Completing Work

When work is ready, report the files changed and the validation that passed.
Leave changes uncommitted and unpushed so the human can perform those operations.
Do not proactively ask to commit or push. If the human directly asks the agent
to perform either operation as a rare exception, follow the two-step
authorization process above.

If requested, provide the human with relevant manual commands, such as:

```powershell
git checkout main
git merge --squash <branch_name>
git commit -m "<descriptive message>"
git push origin main
```

Close the worktree's editor window before removing it on Windows.
