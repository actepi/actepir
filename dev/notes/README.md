# Handoff notes

There is one file of notes per tool or feature of actepir, so that the next session or developer can pick up where the last one left off. Each file records what the code and the pull requests do not make obvious:
- the decisions and their reasons;
- the open items;
- how to check a change.

| Notes | Covers |
|---|---|
| [shared-apps.md](shared-apps.md) | What the Shiny apps share, their add-ins, and how to add a tool |
| [dsr-calculator.md](dsr-calculator.md) | The DSR Calculator (`episerver_dsr()`), `dsr_calculate()` and `dsr_age_specific()` |

The rest of the package has no notes yet: EpiServer access and labels, the ggplot2 and gt themes, and the Quarto and R Markdown templates. Start notes for a part the next time it changes.

[../harness/README.md](../harness/README.md) describes how to run the apps and the live tests offline. [../reference/app-style.md](../reference/app-style.md) is the style guide for the apps' layout, spacing and components.

## Using and keeping the notes

- **Before and after a change.** Read the notes for a tool before changing it, and update them in the same pull request as the change.
- **Audience.** Write for someone who knows R and CLAUDE.md, but did not see the conversation that built the tool.
- **Decisions.** Give each decision its reason and the alternatives rejected, so that it is not reopened by accident. When a decision changes, rewrite its entry; the pull requests keep the history.
- **Open items.** Keep them current, and mark the ones waiting on the maintainer.
- **No repetition.** Point to the code, documentation and pull requests rather than repeating them.

## Template for a new tool

Copy this into `dev/notes/<tool>.md` and add a row to the table above.

```markdown
# <Tool name>

<What the tool does, for whom, and how it is opened.>

## Status

<Merged pull requests, one line each, and any work in progress.>

## Where the code is

<Files, main functions and tests.>

## Decisions

<Each decision with its reason and the alternatives rejected.>

## Open items

<Follow-ups, known limitations and questions waiting on the maintainer.>

## How to check a change

<Offline: the tests and harness runs that cover the tool.
Against EpiServer: what the maintainer runs.>
```
