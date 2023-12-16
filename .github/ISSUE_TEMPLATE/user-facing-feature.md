---
name: User facing feature
about: For internal use only; Open a user-facing feature tracking file
title: "[FEATURE] - "
labels: ["type: internal-feature", "user type: internal"]
assignees: ''

---

## What
<!-- What the end user will see -->

## Why
<!-- The reason why this user-facing feature is being implemented -->

## Personas
<!-- Who will this affect? -->

- [ ] SPOs
- [ ] dApp Devs
- [ ] Exchanges
- [ ] Wallets
- [ ] 3rd party tools
- [ ] ADA holders 

## Definition of Done (DoD)

- [ ] [Acceptance Criteria + User Stories & DoD](https://input-output.atlassian.net/wiki/spaces/QA/pages/3584229468/Acceptance+Criteria+DoD) created and singed-off (by PO, dev & test owners)
- [ ] Builds successfully on CI
- [ ] Code & Test review (as per Acceptance Criteria)
- [ ] There is documentation and/or examples for the new functionality (usage/response)
- [ ] Log/record changes on **Vnext** (or similar depending on what we adopt)
- [ ] Ticket number(s) included in PR description
- [ ] All Acceptance Criteria met and covered by dev/unit/property/integration tests
- [ ] System/E2E automated tests + System Test Engineer Owner Sign-off

> **_NOTE:_**  Ideally, we should merge only fully implemented and tested features into the master branch. 
> So all the above steps are required for the PR to be merged.  
> In order to avoid the PRs becoming stale and requiring to be rebased on master, these can be merged 
> after a reasonable time (current agreement is 3 days) if the System Test Engineer Owner's sign-off 
> was not provided (last step in the DoD).

> **_IMPORTANT:_** Any deviation from the plan should be discussed and agreed as a comment in the Feature file. 

## Sign-off

- [ ] Product Owner
- [ ] Dev Owner
- [ ] System Test Engineer Owner

# Related PRs

1.  PR # here

## Acceptance Criteria

Acceptance Criteria & User Stories define here (or in a separate file (linked here) for a big feature)

Example - https://github.com/intersectmbo/cardano-node/issues/4453
