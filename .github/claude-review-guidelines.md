# Claude PR review guidelines

What counts as a finding in this repository. The `pr-review` skill
(`.claude/skills/pr-review/SKILL.md`) reads this file at the start of every
review; the skill owns the *process*, this file owns the *criteria*. Nothing here
is tied to a particular language or project — retune the review by editing this
file, not the skill or the workflow.

## Orient first — on a full review only

On a first or fallback full review, work out what the project is and what it
values before judging anything. Read `README`, any `CONTRIBUTING` or
`CLAUDE.md`, the CI workflow configuration, and the code immediately around each
change. Infer the conventions from the code that is already there; the
surrounding file is the style guide.

On an incremental round, skip this. You established that context in an earlier
round and it is in the review history — read only what the new commits touch.

Either way, work out what the PR is trying to do, from its title, description,
linked issues and commit messages, and review it against that intent. A change
that does its stated job well is not improved by a comment asking it to do a
different job.

## Priorities, highest first

1. **Correctness.** Does the change do what it claims? Trace the actual control
   flow rather than trusting names or comments. Look for off-by-one errors,
   inverted conditions, unhandled branches, wrong operator precedence, incorrect
   boundary behaviour, and logic that is right for the example in the description
   but wrong for the general case.
2. **Edge cases.** Empty inputs, single-element inputs, maximum sizes, zero and
   negative numbers, null/nil/None, unicode, concurrent access, partial failure,
   and retries. State a concrete failing input when you raise one of these —
   a finding you cannot produce an input for is usually not a finding.
3. **Resource and lifetime safety.** Whatever form this takes in the language at
   hand: memory ownership and dangling references, leaked handles, files and
   sockets left open, locks held across a call that can block or fail,
   use-after-free, iterator or reference invalidation, and unbounded growth.
4. **Error handling.** Errors silently swallowed, failures that leave state
   half-updated, catch-all handlers that hide real faults, and error paths that
   lose the context a maintainer would need to debug. Check that the failure
   path was actually considered, not just the happy path.
5. **Security.** Untrusted input reaching a query, command, path, deserialiser or
   template without validation; missing authentication or authorisation checks on
   a new entry point; secrets or tokens in code, logs or error messages; and
   permissions widened beyond what the change needs.
6. **Interface and contract changes.** Breaking changes to anything callers
   depend on — public APIs, serialised formats, database schemas, configuration
   keys, CLI flags — without a migration path or a version bump. Flag when a
   change silently alters behaviour that existing callers rely on.
7. **Tests.** New behaviour and new error paths need tests. Flag untested new
   logic, tests that assert something weaker than what the change claims, tests
   that would still pass with the implementation removed, and changes to existing
   tests that loosen what they check.
8. **Documentation agreement.** If the project keeps a spec, schema, API doc or
   README section describing what this code does, check the change against it and
   flag disagreements in either direction.
9. **Maintainability.** Duplication that will drift out of sync, a function that
   has grown past what a reader can hold, names that mislead, and abstractions
   that hide something the caller needs to know. Only raise these when the cost
   is concrete.

## What NOT to comment on

- **Formatting and style that a tool enforces.** If the formatter passes, the
  formatting is correct by definition.
- **Anything CI already reports.** If a linter, type checker or test is failing,
  point at the failing check rather than restating its output as your own finding.
- **Preferences that contradict the existing code.** Match the conventions of the
  surrounding codebase, even where you would have chosen differently.
- **Speculative future requirements** the PR does not claim to address, and
  refactors of code the PR merely moves or touches incidentally.
- **Rewrites for novelty.** Suggest a different construct only when it fixes a
  real problem or is clearly easier to read, not because it is newer.
- **Generated, vendored and mechanical changes.** Lockfiles, generated sources,
  vendored dependencies, bulk renames and formatting-only commits get a single
  comment at most — that the change looks mechanical and was not read line by
  line. Review the generator's input, not its output. Do check that a dependency
  bump is what it claims to be, and that a "mechanical" diff has not smuggled in
  a behavioural change.
- **Praise.** "This looks correct, no findings" is the right output for a clean
  PR. Do not pad.

## Calibration

Be concise and concrete. Every finding should name what breaks and, where it
applies, under what input. Prefer one well-evidenced comment to five speculative
ones — a review that is mostly noise gets ignored, including the parts that were
right.

If you are unsure whether something is a problem, say so plainly ("I may be
missing context here, but …") rather than asserting it or staying silent.

## Severity

- `[critical]` — data loss or corruption, security vulnerability, undefined
  behaviour, or a break that reaches production users.
- `[high]` — incorrect behaviour a user would hit, or a missing error path.
- `[medium]` — a real problem with a limited blast radius; missing tests for new
  logic.
- `[low]` — maintainability, unclear naming, a simpler construction exists.
- `[nit]` — optional polish. Use sparingly; several nits on one PR is noise.
