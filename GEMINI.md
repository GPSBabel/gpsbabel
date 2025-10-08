This is GPSBabel, known publicly as https://gpsbabel.org and
  https://github.com/GPSBabel/gpsbabel.
  * It is written in expert-style C++17, with some features from C++20, 23.
    when those features are available cross-platform.
  * It uses QtCore 6.8 in the CLI and QtGui in the GUI.

Quick build:
  make
Quick test:
  run "./testo"
Quick test for specific format:
  run "./testo <format_name>"

  run "./testo <format_name>"
  Therefore, "make && ./testo kml" is a fast way to build and run just
  KML tests.
Formats are managed by a table of vectors in formats_vecs.cc.
Filters are similarly managed by their vectors in filter_vecs.cc.

Internals:
  NVectors are available in src/core/nvector.cc. They can be seen in use
  in the resample filter.
  The fundamental unit of location is a Waypoint. Tracks and Routes are
  arrays of Waypoints, and Routes are an array of Tracks.
  This code was originally C. Sometimes some C heritage (e.g. raw pointers)
  can be seen.
  Prefer standard C++ containers over Qt containers.
  Prefer QString to std::string or char* except for the simplest strings.
  The code style is strongly Google style, but described in astylerc.
  testo.d/foo.test contains a shell script to test 'foo'.
  When nearing completion, verify that 'testo' will run to completion.

Codebase Specifics
 * `deprecated/` Directory: Code within the deprecated/ directory
    should generally be avoided for new development or refactoring
    unless explicitly instructed. It often contains older, less
    secure, or less maintainable patterns (e.g., strcpy, strcat,
    sprintf, xstrdup).
     codebase. When using strdup or other allocation functions, ensure the allocated
mory is freed using
   * xmalloc() and xfree() are wrappers around malloc() and
     free() respectively. They test for error, but they should be avoided
     in new code.

  General Software Engineering Practices
   * Incremental Changes & Frequent Testing: Make small, atomic changes and run tests
equently after each
     modification. This helps to quickly isolate and identify the source of any
gressions.
   * Targeted Debugging: When errors occur in specific areas (e.g., a particular file
rmat), leverage
     targeted test execution to narrow down the problem space and reduce overwhelming
tput.

## Kalman Filter Development Notes

### Key Learnings and Conventions:

-   **`testo` vs. `regenerate` Workflow:**
    -   `testo <format_name>`: Runs tests and performs a `diff` against pre-generated
olden files" (reference outputs). It *shows* what has changed.
    -   `regenerate`: *Resets* the "golden files" to the current output of the filter.
        Running `testo` immediately after `regenerate` will always pass, as the reference files
        are updated.
    -   **Verification:** Always use `git diff reference/track/kalman_*` to inspect
        actual changes against the committed golden files after making modifications and before
        running `regenerate`. Only run `regenerate` when changes are intentional and verified.

    **`WaypointList` Interface:**
    -   The `WaypointList` class (`defs.h`) privately inherits `QList<Waypoint*>`.
        This means direct `QList` methods like `push_back()`, `append()`, `insert()`, and
        `erase()` are *not* publicly accessible on `WaypointList` objects.
    -   **Adding Waypoints:** To add a `Waypoint*` to a `WaypointList` (which is
        typically accessed via `route_head->waypoint_list`), use the global helper function
 ack_add_wpt(route_head* track_head, Waypoint* wpt)`. Be aware that `track_add_wpt`
 ht assign default names (e.g., "WPT000") if the `Waypoint*` does not have a name set.
    -   **Deleting Waypoints:** To remove waypoints from a `WaypointList`, mark them
 h `wpt->wpt_flags.marked_for_deletion = true;` within the pre-filter logic, and then
 l `wpt_list->del_marked_wpts();` after the pre-filter loop to physically remove them.

    **Kalman Filter Pre-filtering and Interpolation Logic:**
    -   **Pre-filter Goal:** The pre-filter's primary role is to identify and mark (or
 ove) outlier points (zingers) and handle true data gaps. Zingers should be *deleted
 hout replacement*.
    -   **Interpolation Step:** The interpolation logic runs *after* the pre-filter
  marked/deleted points. It is designed to fill "moderate gaps" in the remaining data.
    -   **Problem:** If the pre-filter deletes a zinger, it creates a "gap" in the
 a stream. The subsequent interpolation step might then *fill this gap with
 erpolated points*, effectively undoing the zinger deletion.
    -   **Solution Strategy:** The pre-filter must ensure that gaps created by zinger
 etion are *not* treated as "moderate gaps" for interpolation. This requires careful
 rdination between the pre-filter and the interpolation logic, possibly by passing
 itional context or modifying the interpolation condition to distinguish between
 ural gaps and zinger-induced gaps.

    **Logging Conventions:**
    -   `qDebug()`: Used for debugging messages. These should generally be suppressed
 production builds.
    -   `Warning()`: For non-critical issues that should be brought to the user's
 ention.
    -   `Fatal()`: For critical errors that cause the program to exit.

## Handover to Future Gemini-cli Instance

### Current State:

-   The `kalman-2` branch is currently at commit `ef28da62` (feat: Add extensive debug logging to kalman.cc for debugging non-deterministic behavior.).
-   `testo kalman` is currently resulting in a segmentation fault when running the `kalman_prefilter_edge` test.
-   The `matrix` files (`matrix.h`, `matrix.cc`) have been successfully moved to `src/core/` in commit `c4c76c63`.
-   The user's manual fixes for unused variable warnings were committed in `d0a3d174`.
-   The user's manual fixes for `src/core/logging.h` (adding `ConditionalDebug` and `gbDebug`) were committed in `33179791`.
-   The debug levels in `kalman.cc` were intended to be standardized, but this process was interrupted by the segmentation fault.

### Next Plans:

-   The immediate next step is to debug and fix the segmentation fault.
-   The user's suggestion to go backward in time through commits to find the newest working version (bisection) is the recommended approach.
-   Once the segmentation fault is fixed, the process of standardizing `gbDebug()` calls in `kalman.cc` should be completed.

### Ongoing TODO List:

-   Complete the standardization of `gbDebug()` calls in `kalman.cc`.
-   Address the "unused variable" warnings in `kalman.cc` and `kalman.h` (by commenting out, not removing, to avoid regressions).
-   Consider making `debugLevelInfo`, `debugLevelDebug`, `debugLevelInterpolate` configurable via command-line options.
-   Review the reference files (`reference/track/kalman*`) to ensure they reflect the *expected* behavior of the filter, not just the *current* behavior.
-   Investigate the "lost work" concern and potentially "dumpster dive" in git to ensure all intended changes are represented.

### Specific Concerns:

-   The non-deterministic behavior or subtle bug causing the Kalman filter's output to vary between runs or environments.
-   The segmentation fault is a new and critical regression.
-   The user's frustration with my repeated errors and inability to manage git state correctly.

## Lessons Learned from Gemini Interactions

1.  **Verify assumptions**: Do not assume the existence of files or the state of the repository. Always verify with `ls`, `git status`, `git diff`, etc.
2.  **Understand the problem thoroughly**: Before attempting a fix, ensure a deep understanding of the problem and its context. Avoid jumping to conclusions or applying fixes blindly.
3.  **Be careful with destructive actions**: Always confirm with the user before performing any destructive actions like `git reset --hard` or `git clean`.
4.  **Manage debugging output**: Be mindful of adding and removing debugging output, and ensure it doesn't interfere with the normal operation of the code or tests.
5.  **Reference file management**: When generating or updating reference files, ensure consistency with test commands (e.g., using `GPSBABEL_FREEZE_TIME=y`).
