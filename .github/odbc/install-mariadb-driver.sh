#!/usr/bin/env bash
set -ex

# Install the unixODBC driver manager (headers + runtime) and the MariaDB
# Connector/ODBC driver from the Ubuntu package repositories.
#
# This previously downloaded prebuilt tarballs from downloads.mariadb.com, but
# those URLs now return HTTP 522 and the pinned "focal" builds are incompatible
# with the ubuntu-24.04 runner.  The distro packages are self-contained and
# maintained:
#   * unixodbc-dev provides sql.h, needed to compile the odbc R package.
#   * odbc-mariadb installs libmaodbc.so to /usr/lib/x86_64-linux-gnu/odbc/ and
#     pulls in libmariadb3, which ships the authentication plugins (including
#     caching_sha2_password, required to connect to MySQL 8) in its default
#     plugin directory, so no manual plugin installation is required.
sudo apt-get update
sudo apt-get install -y unixodbc-dev odbc-mariadb

# Confirm the driver is present and its shared-library dependencies resolve.
ldd /usr/lib/x86_64-linux-gnu/odbc/libmaodbc.so
