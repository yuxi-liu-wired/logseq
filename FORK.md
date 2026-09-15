This fork exists to make typing in the Logseq DB version fast on a large graph. The branch `fork/main` carries every change on top of upstream `master`, and this file records what the changes are, how to rebase them onto a newer upstream, how to build a runnable desktop app from them, and how to measure that they still work.

Upstream base: `logseq/logseq` commit `be800f1`, which equals the nightly build of 2026-09-08. Store dependency: this fork's copy of `logseq/rfx` at `https://github.com/yuxi-liu-wired/rfx`, branch `perf/incremental-subscription-updates`, commit `3dbe738`, pinned in `deps.edn`.

The problem being fixed: on a graph of about 2,000 pages and 150,000 blocks, each keystroke took 72 ms from key press to paint (p90 112 ms) and one key in four stalled the main thread for over 50 ms. A CPU profile put 36 percent of the time inside the state store's `next-state!`, which re-evaluated every mounted subscription on every state write, and each keystroke wrote the store five times. The rest was the editor box re-rendering on every key: the autosize textarea re-measuring, a hidden one-span-per-character mirror of the text being rebuilt, and a command-trigger check running as a render effect.

The changes, one branch each, all branched from `be800f1`:

- `perf/editor-state-atoms` (`src/main/frontend/state.cljs`, `components/block/comments.cljs`): last input time, last key code, saved cursor, and the UI's last key code move out of the reactive store into plain atoms. Nothing subscribed to them.
- `perf/editor-box-rerender` (`components/editor.cljs`, `handler/editor.cljs`, `util/cursor.cljs`): the editor box reads the block text once instead of subscribing; the heading class is set on the DOM node from the change handler; the caret mirror is built on demand only when a caret position is needed; the first-row and last-row checks read one `offsetTop`; the command-trigger check runs from the change handler.
- `perf/textarea-field-sizing` (`ui.cljs`): a plain textarea sized by CSS `field-sizing: content` where `CSS.supports` reports it (Chromium 123, Safari 26.2, Firefox 152), and `react-textarea-autosize` elsewhere.
- `perf/store-changed-paths` (`rfx.cljs`, `deps.edn`): state writes pass the changed path to the store, and a mapping from each subscription to the state path it reads, so the store re-evaluates only the subscriptions reading that path. Depends on the rfx change below.
- `fix/file-test-redefs` (`src/test/frontend/components/file_test.cljs`): a test signalled done before its stubs were restored, which made a later test in the same process fail. Test-only.
- In the rfx fork, `perf/incremental-subscription-updates` (`modules/rfx/src/io/factorhouse/rfx/store.cljc`, `stores/atom.cljc`, `test/rfx/store_incremental_test.clj`): the atom store indexes cached subscriptions by the state paths they read and, on a write, evicts and recomputes only those under the changed paths. Without a `!sub-key-fn` it behaves as before.

Measured result on the same graph, key press to next frame, p50 / p90 / max over 83 keys, in the box on a virtual display: journals page 15.7 / 25.2 / 45 before, 4.3 / 6.2 / 14.1 after; a 204 KB page 44.7 / 50.7 / 66 before, 8.7 / 10.5 / 13.7 after; a 337 KB page 27.9 / 32 / 43 before, 4.5 / 11.1 / 29.9 after. Key handler time per key on the 204 KB page: 11 ms before, 0.7 ms after. Block-to-block arrow moves: 44 ms with 38 stalls in 40 moves before, 21 ms with none after.

Rebasing onto a newer upstream, from a clone with `origin` = `logseq/logseq` and `fork` = this repository:

```bash
git fetch origin master
git checkout fork/main
git rebase origin/master
```

The five commits touch eight files. Conflicts, when they come, will be in `components/editor.cljs` and `state.cljs`, which upstream edits often; resolve by keeping the mechanism described above rather than the exact lines. After the rebase, run the checks below before building.

Checks, from the repository root, one process at a time and never on a machine that is also running the desktop:

```bash
pnpm install
clojure -M:clj-kondo --parallel --lint src --cache false
pnpm cljs:test
pnpm cljs:run-test
```

The unit runner groups namespaces into shared node processes; with `fix/file-test-redefs` included every group passes. One wall-clock assertion in `frontend.worker.search-test` (300 blocks indexed under 1000 ms) measures total process CPU and fails on a loaded machine; rerun it alone.

Building a runnable desktop app. The nightly AppImage is used as the shell and only the renderer files are replaced, because the worker script shipped in the nightly is a standalone bundle and the main process refuses a worker from a different build revision.

```bash
pnpm release
```

That writes `static/js/main.js` and the other renderer files. Then, with the matching nightly AppImage extracted (`./Logseq-linux-x86_64-<version>.AppImage --appimage-extract` gives `squashfs-root/`) and its archive unpacked (`npx @electron/asar extract squashfs-root/resources/app.asar app`):

```bash
rsync -a --exclude "*.map" --exclude node_modules static/js/ app/js/
cp static/index.html app/index.html
rsync -a --exclude "*.map" static/css/ app/css/
```

Keep the nightly's own `app/electron.js`, `app/db-worker-node.js`, `app/js/db-worker-node.js`, `app/js/db-worker.js`, `app/js/db-worker-bundle.js`, and `app/logseq-cli.js`. Then repack and run:

```bash
npx @electron/asar pack app squashfs-root/resources/app.asar --unpack-dir "{node_modules/**/*.node,**/*.node}"
./squashfs-root/AppRun --no-sandbox
```

If a later change touches the worker, build its standalone bundle with `pnpm db-worker-node:release:bundle` and use `dist/db-worker-node.js` for both worker paths in the archive.

Measuring that typing is still fast, without touching a real window: launch the app with `--remote-debugging-port=9333`, then attach a recorder that reads the browser's Event Timing entries for `keydown` and `input` and its long tasks, and type into a page. The scripts used for the numbers above live in the `logseq-perf` workspace: `scripts/lsq-bench.mjs` (scripted typing over the DevTools protocol, reports p50 / p90 / max and long tasks), `scripts-host/install.mjs` and `read.mjs` (a persistent passive recorder for a person's own typing), `scripts-host/profile.mjs` and `analyze.cjs` (a CPU profile resolved to ClojureScript source through `js/main.js.map`). The acceptance line is p90 at or under 16 ms with zero long tasks during a typing burst on the largest pages.

Known remaining costs, measured and left alone: opening a 204 KB page costs a 632 ms render task and a 318 ms first layout before typing begins; V8 major garbage collections of 36 to 74 ms happen in the seconds after a page opens and never during typing.

The graph itself lives outside the app, under `~/logseq/graphs/<name>/db.sqlite`, with a worker daemon per open graph that outlives the app; a stale `db-worker.lock` there blocks a relaunch until the process it names is gone. The app's list of graphs is in `~/.config/Logseq/Local Storage/leveldb`. Markdown files are produced from the database by the app's Markdown Mirror setting (Settings, Features), into `~/logseq/graphs/<name>/mirror/markdown`.
