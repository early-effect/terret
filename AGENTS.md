# Terret

Harness, transport, permission, and launch work is not done until the live grok smoke test has been run on this machine and its output pasted.

```text
./histos test grok --only terret.grok.LiveGrokSpec 2>&1 | tee /tmp/terret-live-grok.log
```

Keep that log. Do not redirect it away.
