## Phase 1: Zero-Shim STL Replacement Sweep

Mechanically replace legacy Qt containers with standard C++17/20 STL constructs across the entire codebase. These require zero custom wrapper code in `gbtypes.h`:

| Legacy Qt Type | Standard C++ Replacement | Notes / Guidelines |
| :--- | :--- | :--- |
| `QPair<T, U>` | `std::pair<T, U>` | Direct replacement. |
| `QVector<T>` / `QList<T>` | `std::vector<T>` | Ensure random-access iteration assumptions hold. |
| `QMap<K, V>` | `std::map<K, V>` | Sorted key-value store. |
| `QHash<K, V>` | `std::unordered_map<K, V>` | Hash table; ensure `std::hash` specialization exists. |
| `QVariant` | `std::variant<...>` / `std::any` | Prefer explicit `std::variant` unions over `any`. |
| `QFileInfo` / Path parsing | `std::filesystem::path` | Use `.stem()`, `.extension()`, `.parent_path()`. |
| Raw Data Buffers | `std::vector<uint8_t>` / `std::span` | Avoid `QByteArray` where string ops are unnecessary. |

---

## Phase 2: Canonical `gbtypes.h` & Independent TDD Suite

Implement `gbtypes.h` as a standalone contract decoupled from format quirks, accompanied by a dedicated test executable (`tests/test_gbtypes.cc`).

### 1. `gb::string` Contract
* Derives from or cleanly wraps `std::string`.
* Exposes explicit manipulation methods: `.trimmed()`, `.split()`, `.toLower()`, `.toUpper()`, `.arg()`.
* Provides numeric conversion: `.toInt()`, `.toDouble()`, `.toLongLong()`, `.toULongLong()`.
* Restricts equality operators (`==`, `!=`) strictly to `std::string`, `std::string_view`, and `const char*` to prevent ambiguous conversion warnings.

### 2. `gb::Date`, `gb::Time`, `gb::DateTime` Contract
* Clear, standard class interfaces with uniform accessor methods: `.year()`, `.month()`, `.day()`, `.hour()`, `.minute()`, `.second()`, `.msec()`.
* Explicit epoch methods: `.toSecsSinceEpoch()`, `.toMSecsSinceEpoch()`, `.fromSecsSinceEpoch(val, spec)`.
* Robust `::fromString(str, fmt)` parsing supporting ISO-8601 and common GPS text format masks.

### 3. Standalone Unit Tests (`tests/test_gbtypes.cc`)
* String substitution edge cases (`%1` positional markers, zero-padding, hex formatting).
* Subsecond timestamp precision and ISO-8601 round-tripping.
* Timezone handling: explicit UTC vs. Local Time epoch transitions without implicit offset leakage.

---

## Phase 3: The 10-Format Diversity Cohort & TDD Loop

Port these 10 formats in strict sequence to exercise distinct data structures:

| Order | Format | Primary Architectural Stress Point | Validation Target |
| :--- | :--- | :--- | :--- |
| 1 | **`unicsv`** | Delimited text, geocache tags | String splitting, basic date/time formatting |
| 2 | **`ozi`** | Fixed text records, route headers | Route/track structures, integer parsing |
| 3 | **`nmea`** | Sentence streams, sequential checksums | Subsecond math, floating point speed/coords |
| 4 | **`exif`** | Binary byte buffers, EXIF metadata | Rational time structures, GPS datestamp strings |
| 5 | **`garmin`** | Proprietary binary protocols | Raw buffer packing, bitmask handling |
| 6 | **`csv`** | Tabular records | Custom field mapping, escape handling |
| 7 | **`glog`** | Fixed text logging | Minimalist trackpoint ingest |
| 8 | **`magellan`** | Serial/sentence protocol | Legacy GPS coordinate conversion |
| 9 | **`shapefile`** | Binary geometric vector data | Multi-table DBF record parsing |
| 10 | **`humminbird`**| Binary sonar/navigation records | Endianness handling, custom binary structs |

### The TDD Workflow:
1. **Target Single Format:** Port call sites in `src/formats/<format>.cc` to match the `gbtypes.h` API.
2. **Handle Incompatibilities:**
   * If the format uses legacy/deprecated syntax -> fix the format call-site.
   * If a standard utility is missing from `gbtypes.h` -> add a failing test to `test_gbtypes.cc`, implement the utility in `gbtypes.h`, and verify the unit test passes.
3. **Validate:** Execute `./testo <format>`. Debug until the test matches reference output perfectly with zero warnings.
4. **Commit:** Check in the working format changes before moving to the next.

---

## Phase 4: Quarantine Zone (Complex Subsystems)

Defer these subsystems until the 10 cohort formats pass completely:

* **XML Formats (`gpx`, `kml`):** Replace `QXmlStreamReader` and `QDomDocument` with a dedicated, lightweight C++ XML library (e.g., `pugixml`).
* **JSON Formats:** Replace `QJson*` with `nlohmann/json` or `glaze`.
* **Localization / Number Formatting:** Replace `QLocale` decimal and thousand separators using `std::from_chars` and standard `<locale>`.
* **Application Framework:** Remove `QCoreApplication` and replace event loops or CLI arg processing with standard C++ entry points.

---

## Tooling & Agent Execution Roles

* **Jules (Headless Cloud Agent):** Execute Phase 1 mechanical search-and-replace tasks across the repo, followed by isolated single-format porting PRs in Phase 3.
* **Antigravity IDE / Local Workspace:** Develop and refine Phase 2 core architecture (`gbtypes.h` and `test_gbtypes.cc`), and debug low-level binary test failures.
