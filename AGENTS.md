# Terret

Do not run `terret restart`, `terret-off`, `terret-on`, or `launchctl bootout` / `kickstart` / `kill` against the live launchd jobs (`rocks.earlyeffect.terret` and `rocks.earlyeffect.terret-tunnel`). This session is served by that process. Restart is tested through stubs.

Run only the tests a change touches.

`./histos test <project> --only <suite>` selects suites by name. `--tag <name>` keeps a test that carries that exact tag. `--test <term>` keeps a test whose label contains the term. `--quick` skips a suite whose stamp still matches, and it is not a full proof.

Do not run `LiveGrokSpec` or `LiveRefusalSpec` as a whole, including before a push. A live case that must run alone carries a tag. The MCP opt-in smoke is `mcp-optin`.

This histos copies `CI` and `GITHUB_ACTIONS` into the test process. `LiveGate` skips the live suite when either is set.

Tee the log under `target/`. Do not pipe it away.

```text
set -o pipefail
./histos test grok --only terret.grok.PermissionChoiceSpec 2>&1 | tee target/permission-choice.log
./histos test grok --tag mcp-optin 2>&1 | tee target/mcp-optin.log
```
