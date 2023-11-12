# Notice about Mallard project

This repository uses `git subtree` to manage Mallard utility located in a separate repository. Below are the instructions for developers on how to interact with these subtrees.

## Prerequisites

Ensure you have Git installed on your system and you have sufficient permissions to push to the main project repository and the subtree repository.

## Adding the Subtree

If the subtree has not been added to the project yet, go to the root of the Dev repo and use the following command to add it:

```bash
git subtree add --prefix=Utils/mallard git@github.com:dymium-io/mallard.git main
```

## Pulling Changes from the Subtree

To pull the latest changes from the subtree repository into your project, use:

```bash
git subtree pull --prefix=Utils/mallard git@github.com:dymium-io/mallard.git main
```

## Pushing Changes to the Subtree

If you've made changes to the subtree within the main project and want to push these changes back to the subtree's repository, use:

```bash
git subtree push --prefix=Utils/mallard git@github.com:dymium-io/mallard.git main
```
