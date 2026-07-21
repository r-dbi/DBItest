set -ex

# Install the MariaDB ODBC driver from the Ubuntu archive.
# This pulls in libmariadb3, which ships the connector-C authentication
# plugins (e.g. caching_sha2_password, needed to authenticate against MySQL 8)
# in its own default plugin directory, so no manual plugin placement is needed.
# The driver is installed at /usr/lib/x86_64-linux-gnu/odbc/libmaodbc.so, which
# the "MySQL Driver" entry in .github/odbc/odbcinst.ini points to.
#
# This replaces a manual download from downloads.mariadb.com that pinned an old
# focal-only build (3.1.9) and had become unreliable (the CDN returned HTTP 522,
# leaving libmaodbc.so uninstalled and the backend tests unable to connect).
sudo apt-get install -y odbc-mariadb
