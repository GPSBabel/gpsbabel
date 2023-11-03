//  Temporary wrapper to hide a standard installatino of libfmt and
//  pretend it's in the std namespace.
//
//  MacOS  14.1 has the header (yay!) but doesn't have an implementation
//  in the clang libc++ (booo!) so we test for libfmt first.
//  GCC 13.1, available from Homebrew, alledgely does support std::format
//  but we'll try the library approach first as it keeps us more consistent
//  with what normals will have.
//
//  *-- Since we're shimming things anyway, we'll go ahead and implement
//  gb::print and gb::println that mirror C23's versions from std::. --*
//  We'll unabashedly use print() from C23 from fmt/ostream. It'll be
//  trivial to mimic on Linux if we have to.

#if __has_include(<fmt/format.h>)

#include <iostream>
#include <fmt/format.h>
#define _USING_LIBFMT 1

namespace std
{
  using fmt::detail::type_is_unformattable_for;
  using fmt::detail::compile_parse_context;

  using fmt::format;
  using fmt::format_to;
  using fmt::format_to_n;
  using fmt::formatted_size;
  using fmt::formatter;
  using fmt::formatter;
  using fmt::format_error;

  using fmt::basic_format_arg;
  using fmt::basic_format_args;
  using fmt::basic_format_context;
  using fmt::make_format_args;
  using fmt::format_args;

  using fmt::print;
  using fmt::println;

  using fmt::vformat;
  using fmt::vformat_to;
} ;

// Fallback to the system version.
#elif __has_include(<format>)
# include <format.h>
#else
# warning "There is no <format.h>"
#endif

#if  _USING_LIBFMT
// This allows you to send QStrings to a fmt::format. Ha!
template <>
struct fmt::formatter<QString> : public fmt::formatter<std::string> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext &ctx) {
    return fmt::formatter<std::string>::parse(ctx);
  }

  template <typename FormatContext>
  auto format(QString const &val, FormatContext &ctx) {
    return fmt::formatter<std::string>::format(val.toStdString(), ctx);
  };
};
#endif //  _USING_LIBFMT
