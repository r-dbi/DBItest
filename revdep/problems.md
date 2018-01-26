# MonetDBLite

Version: 0.5.0

## In both

*   checking whether package ‘MonetDBLite’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/muelleki/git/R/DBItest/revdep/checks/MonetDBLite/new/MonetDBLite.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘MonetDBLite’ ...
** package ‘MonetDBLite’ successfully unpacked and MD5 sums checked
** libs
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/common/mutils.d -c monetdblite/src/common/mutils.c -o build/objects/monetdblite/common/mutils.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/common/mutils.o' failed
make: *** [build/objects/monetdblite/common/mutils.o] Error 1
make: *** Waiting for unfinished jobs....
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_align.d -c monetdblite/src/gdk/gdk_align.c -o build/objects/monetdblite/gdk/gdk_align.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_align.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_align.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_atoms.d -c monetdblite/src/gdk/gdk_atoms.c -o build/objects/monetdblite/gdk/gdk_atoms.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_atoms.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_atoms.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/common/stream.d -c monetdblite/src/common/stream.c -o build/objects/monetdblite/common/stream.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/common/stream.o' failed
make: *** [build/objects/monetdblite/common/stream.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/embedded/embedded.d -c monetdblite/src/embedded/embedded.c -o build/objects/monetdblite/embedded/embedded.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/embedded/embedded.o' failed
make: *** [build/objects/monetdblite/embedded/embedded.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_bat.d -c monetdblite/src/gdk/gdk_bat.c -o build/objects/monetdblite/gdk/gdk_bat.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_bat.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_bat.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_batop.d -c monetdblite/src/gdk/gdk_batop.c -o build/objects/monetdblite/gdk/gdk_batop.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_batop.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_batop.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_aggr.d -c monetdblite/src/gdk/gdk_aggr.c -o build/objects/monetdblite/gdk/gdk_aggr.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_aggr.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_aggr.o] Error 1
ERROR: compilation failed for package ‘MonetDBLite’
* removing ‘/home/muelleki/git/R/DBItest/revdep/checks/MonetDBLite/new/MonetDBLite.Rcheck/MonetDBLite’

```
### CRAN

```
* installing *source* package ‘MonetDBLite’ ...
** package ‘MonetDBLite’ successfully unpacked and MD5 sums checked
** libs
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/common/mutils.d -c monetdblite/src/common/mutils.c -o build/objects/monetdblite/common/mutils.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/common/mutils.o' failed
make: *** [build/objects/monetdblite/common/mutils.o] Error 1
make: *** Waiting for unfinished jobs....
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_align.d -c monetdblite/src/gdk/gdk_align.c -o build/objects/monetdblite/gdk/gdk_align.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_align.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_align.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_atoms.d -c monetdblite/src/gdk/gdk_atoms.c -o build/objects/monetdblite/gdk/gdk_atoms.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_atoms.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_atoms.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/common/stream.d -c monetdblite/src/common/stream.c -o build/objects/monetdblite/common/stream.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/common/stream.o' failed
make: *** [build/objects/monetdblite/common/stream.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_bat.d -c monetdblite/src/gdk/gdk_bat.c -o build/objects/monetdblite/gdk/gdk_bat.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_bat.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_bat.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/embedded/embedded.d -c monetdblite/src/embedded/embedded.c -o build/objects/monetdblite/embedded/embedded.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/embedded/embedded.o' failed
make: *** [build/objects/monetdblite/embedded/embedded.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_batop.d -c monetdblite/src/gdk/gdk_batop.c -o build/objects/monetdblite/gdk/gdk_batop.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_batop.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_batop.o] Error 1
gcc -std=gnu99  -Wall -pedantic -g -O2 -fdebug-prefix-map=/build/r-base-fCgT8l/r-base-3.4.2=. -fstack-protector-strong -Wformat -Werror=format-security -Wdate-time -D_FORTIFY_SOURCE=2 -g  -I/usr/share/R/include -DNDEBUG -fpic -Wno-format -DLIBGDK -DLIBMAL -DLIBOPTIMIZER -DLIBSTREAM -DHAVE_EMBEDDED_R -DMONETDBLITE_COMPILE -Imonetdblite/src/ -Imonetdblite/src/common -Imonetdblite/src/embedded -Imonetdblite/src/gdk -Imonetdblite/src/mal/mal -Imonetdblite/src/mal/modules -Imonetdblite/src/mal/optimizer -Imonetdblite/src/mal/sqlbackend -Imonetdblite/src/sql/include -Imonetdblite/src/sql/common -Imonetdblite/src/sql/server -Imonetdblite/src/sql/storage -Imonetdblite/src/sql/storage/bat -MMD -MF build/deps/monetdblite/gdk/gdk_aggr.d -c monetdblite/src/gdk/gdk_aggr.c -o build/objects/monetdblite/gdk/gdk_aggr.o
cc1: error: -Wformat-security ignored without -Wformat [-Werror=format-security]
cc1: some warnings being treated as errors
Makevars:194: recipe for target 'build/objects/monetdblite/gdk/gdk_aggr.o' failed
make: *** [build/objects/monetdblite/gdk/gdk_aggr.o] Error 1
ERROR: compilation failed for package ‘MonetDBLite’
* removing ‘/home/muelleki/git/R/DBItest/revdep/checks/MonetDBLite/old/MonetDBLite.Rcheck/MonetDBLite’

```
# odbc

Version: 1.1.3

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.2Mb
      sub-directories of 1Mb or more:
        libs   5.9Mb
    ```

# RPostgres

Version: 1.0-3

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        libs   4.7Mb
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘withr’
      All declared Imports should be used.
    ```

# RSQLite

Version: 2.0

## Newly fixed

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(RSQLite)
      > 
      > test_check("RSQLite")
      1. Failure: DBItest[RSQLite]: Connection: cannot_forget_disconnect (@spec-connection-disconnect.R#24) 
      gc() showed 0 warnings
      
      
      testthat results ================================================================
      OK: 4255 SKIPPED: 15 FAILED: 1
      1. Failure: DBItest[RSQLite]: Connection: cannot_forget_disconnect (@spec-connection-disconnect.R#24) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  9.4Mb
      sub-directories of 1Mb or more:
        libs   8.8Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘pkgconfig’
      All declared Imports should be used.
    ```

# RSQLServer

Version: 0.3.0

## In both

*   checking package dependencies ... ERROR
    ```
    Package required but not available: ‘rJava’
    
    See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
    manual.
    ```

