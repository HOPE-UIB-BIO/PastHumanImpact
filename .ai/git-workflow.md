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
If a commit or push is needed, stop and ask the user to run or approve the exact
git operation.

Recommended manual commands after explicit user approval:

```powershell
git checkout main
git merge --squash <branch_name>
git commit -m "<descriptive message>"
git push origin main
```

Close the worktree's editor window before removing it on Windows.
