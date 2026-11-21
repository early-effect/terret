# Terret

Run only the tests a change touches.

`./histos test <project> --only <suite>` selects suites by name. It does not select one test inside a suite, and histos does not forward a test-name filter. Put a live case you need to run in its own suite, and pass that suite to `--only`. Do not run the whole live suite by default, including before a push. `LiveGrokSpec` is that whole suite. `LiveRefusalSpec` is the refusal cases.

`CI=true` does not reach the test process through histos (histos#212), so it does not skip the live suite.

Tee the log under `target/`. Do not pipe it away.

```text
set -o pipefail
./histos test grok --only terret.grok.PermissionChoiceSpec 2>&1 | tee target/permission-choice.log
```
