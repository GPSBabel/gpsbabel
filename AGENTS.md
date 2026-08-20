# GPSBabel Agent Guardrails & Operating Contract

## 1. Golden Rules
- **Reference Files Are Sacred:** Files under `reference/` are immutable ground truth. NEVER modify a reference file to make a failing test pass. If output does not match `reference/`, your code is wrong.
- **Formats Conform to Core, Not Core to Formats:** `src/core/gbtypes.h` defines the canonical C++20 vocabulary. If a format has archaic syntax (e.g., `t.hour` without parentheses, or homebrewed string pointer slicing), update the format call-site. Do NOT add Frankenstein overloads, dual struct/method semantics, or ambiguous operator conversions to `gbtypes.h`.
- **Zero Warnings:** All builds must compile clean with `-Wall -Wextra -Werror` (or zero compiler warnings).
- Never commit trailing whitespace in any source files.
- Stay out of gui, deprecated, and no commits in reference.

## 2. Build & Test Commands
- **Build Core & Formats:** `cmake --build build -j$(sysctl -n hw.ncpu 2>/dev/null || nproc)`
- **Run Specific Format Test:** `./testo <format_name>` (e.g., `./testo unicsv ozi nmea exif`)
- **Run Standalone Core Unit Tests:** `ctest --test-dir build -R test_gbtypes --output-on-failure`
- **Full Test Suite:** `./testo` (1m22s on a M1-MBP)
- **Full Torture Test Suite:** `./vtesto` (valgrind: many tens of minutes - best left to CI)

## 3. C++20 Idioms
- Use `std::filesystem::path` for all file/path manipulation instead of string slicing or `QFileInfo`.
- Use `std::string_view` for read-only parameter passing.
- Prefer standard algorithms (`std::ranges`, `<algorithm>`) over hand-rolled loops.
