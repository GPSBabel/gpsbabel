#!/bin/bash -e

TARGET_UID=$1
TARGET_GID=$2
TARGET_LANG=$3

# Make sure user and group exist
if ! getent group "${TARGET_GID}" >/dev/null 2>&1; then
  groupadd -g "${TARGET_GID}" gpsbabel_user
fi
if ! getent passwd "${TARGET_UID}" >/dev/null 2>&1; then
  useradd -g "${TARGET_GID}" -u "${TARGET_UID}" -m gpsbabel_user
fi

# Dynamic Locale Generation
if [ -n "${TARGET_LANG}" ] && [ "${TARGET_LANG}" != "C" ] && [ "${TARGET_LANG}" != "POSIX" ]; then
  
  # Skip completely if localedef or the i18n definition tree is missing
  if command -v localedef >/dev/null 2>&1 && [ -d "/usr/share/i18n" ]; then
    
    # Check if the requested locale is already available in the container
    # Run the check under standard 'C' environment to suppress uninitialized locale warnings
    if ! LC_ALL=C locale -a | grep -Fxq "${TARGET_LANG}" && ! LC_ALL=C locale -a | grep -ixq "$(echo "${TARGET_LANG}" | tr -d '-')"; then
      
      echo "Locale '${TARGET_LANG}' not found. Compiling dynamically via localedef..."

      # Extract the base language rule (e.g., 'de_DE' from 'de_DE.UTF-8')
      LOCALE_BASE=$(echo "${TARGET_LANG}" | cut -d'.' -f1)
      
      # Extract the character set format (e.g., 'UTF-8' from 'de_DE.UTF-8')
      # Default to UTF-8 if no dot separation exists in the string
      CHARSET=$(echo "${TARGET_LANG}" | cut -s -d'.' -f2 | cut -d'@' -f1)
      if [ -z "${CHARSET}" ]; then
        CHARSET="UTF-8"
      fi

      # Compile the binary profile cleanly on the fly
      if localedef -i "${LOCALE_BASE}" -f "${CHARSET}" "${TARGET_LANG}" 2>/dev/null; then
        echo "Successfully generated and registered locale: ${TARGET_LANG}"
      else
        echo "Warning: localedef failed to compile layout for ${LOCALE_BASE} with ${CHARSET}."
      fi
    fi
  else
    # Quietly fall back without crashing on older/minimalist images that lack locale files
    echo "Container environment does not support locale compilation. Skipping generation."
  fi
fi
