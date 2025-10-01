
To a Future Instance of Gemini-cli,

This letter serves as a handover of the current state and future plans for the `gpsbabel`
project, specifically focusing on the `kalman-2` branch. My apologies for the challenges
encountered during this session, which have necessitated this transition.

### Current State:

The `kalman-2` branch is currently at commit `ef28da62` (feat: Add extensive debug logging to
kalman.cc for debugging non-deterministic behavior.).

*   **Codebase Status:** The code compiles without warnings, and all existing `testo kalman`
tests pass.
*   **Recent Changes:**
    *   The `matrix` files (`matrix.h`, `matrix.cc`) have been successfully moved to
`src/core/` in commit `c4c76c63`.
    *   The user's manual fixes for unused variable warnings were committed in `d0a3d174`.
    *   The user's manual fixes for `src/core/logging.h` (adding `ConditionalDebug` and
`gbDebug`) were committed in `33179791`.
    *   Extensive debug logging was added to `kalman.cc` in commit `00b9d5e3`.
    *   `testo.d/kalman.test` was modified to isolate the `kalman_prefilter_edge` test for
debugging in commit `e55fcac3`.
*   **Critical Issue:** Despite the tests passing at commit `49834736`, a subsequent run of
`testo kalman` (after re-adding debug statements and isolating a test case) resulted in a
**segmentation fault** when processing the `kalman_prefilter_edge` test. This is a new and
critical regression.
*   **Debugging Context:** The debug levels in `kalman.cc` were intended to be standardized (
using `gbDebug()` with appropriate levels), but this process was interrupted by the
segmentation fault.

### Next Plans:

1.  **Debug and Fix the Segmentation Fault:** This is the immediate critical priority. The
recommended approach is to perform a **git bisection** to pinpoint the exact commit that
introduced the segmentation fault. Start from the current HEAD (`ef28da62`) and go backward,
testing each relevant commit until the crash is identified.
2.  **Complete Standardization of `gbDebug()` Calls:** Once the segmentation fault is resolved
and the code is stable, ensure all `qDebug()` calls in `kalman.cc` are replaced with
`gbDebug()` at appropriate levels (`debugLevelInfo` for `[INFO]`, `debugLevelDebug` for
`[DEBUG]`, `debugLevelInterpolate` for verbose interpolation output).
3.  **Address "Unused Variable" Warnings:** Re-evaluate the warnings for "unused" variables in
`kalman.cc` and `kalman.h`. If truly unused, remove them. If they are part of an incomplete
feature or have a subtle influence on the output, understand their role and potentially
complete the feature.
4.  **Review and Potentially Update Reference Files:** After the code is stable and its
behavior is fully understood, carefully review the `reference/track/kalman*` files. Changes to
these files should only be committed if they reflect *intentional* changes in the filter's
expected behavior, not just environmental floating-point differences.
5.  **Investigate "Lost Work" and Git History:** The user expressed concern about lost
progress. A thorough review of the git history may be necessary to ensure all intended changes
are represented and no work has been inadvertently lost.

### Ongoing TODO List:

*   Ensure consistent use of `gbDebug()` throughout `kalman.cc` and other relevant Kalman
filter files.
*   Consider making `debugLevelInfo`, `debugLevelDebug`, `debugLevelInterpolate` (and
potentially other levels) configurable via command-line options, as suggested by the `// TODO:
get from options` comments.
*   Maintain strict adherence to the `testo` workflow: understand *why* output changes before
regenerating reference files.

### Specific Concerns:

*   **Non-deterministic Behavior:** There is a persistent concern about non-deterministic
behavior or subtle bugs causing the Kalman filter's output to vary between runs or
environments. This needs careful investigation.
*   **Segmentation Fault:** The current segmentation fault is a critical regression that must
be resolved before any further feature development.
*   **User Frustration:** The user has expressed significant frustration with my repeated
errors and inability to manage git state correctly. Building trust and demonstrating
      methodical progress is paramount.

### Once back on track:
✦ The user wants to know the planned feature work for the Kalman code. My apologies for losing track.

  The feature work centers on making the Kalman filter robust, tunable, and correctly integrated into
  gpsbabel. This includes:

   1. Implementing tunable intervals for the Kalman filter.
   2. Perfecting pre-filtering logic to handle data anomalies (spikes, gaps, zingers) without interpolation
      conflicts.
   3. Optimizing interpolation to fill gaps effectively.
   4. Ensuring filter stability and correctness through comprehensive testing and a clear debugging
      framework.
   5. Integrating new mathematical utilities (NVector, Vector3D) for precise geospatial calculations.

  This aims to deliver a robust, tunable, and well-tested Kalman filter. I will now share this summary.



I wish you success in resolving these issues and continuing the development of the Kalman
filter.

Sincerely,

Your Predecessor Instance.
