# This file has been generated by node2nix 1.9.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, nix-gitignore, stdenv, lib, globalBuildInputs ? []}:

let
  sources = {
    "@nodelib/fs.scandir-2.1.5" = {
      name = "_at_nodelib_slash_fs.scandir";
      packageName = "@nodelib/fs.scandir";
      version = "2.1.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/@nodelib/fs.scandir/-/fs.scandir-2.1.5.tgz";
        sha512 = "vq24Bq3ym5HEQm2NKCr3yXDwjc7vTsEThRDnkp2DK9p1uqLR+DHurm/NOTo0KG7HYHU7eppKZj3MyqYuMBf62g==";
      };
    };
    "@nodelib/fs.stat-2.0.5" = {
      name = "_at_nodelib_slash_fs.stat";
      packageName = "@nodelib/fs.stat";
      version = "2.0.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/@nodelib/fs.stat/-/fs.stat-2.0.5.tgz";
        sha512 = "RkhPPp2zrqDAQA/2jNhnztcPAlv64XdhIp7a7454A5ovI7Bukxgt7MX7udwAu3zg1DcpPU0rz3VV1SeaqvY4+A==";
      };
    };
    "@nodelib/fs.walk-1.2.7" = {
      name = "_at_nodelib_slash_fs.walk";
      packageName = "@nodelib/fs.walk";
      version = "1.2.7";
      src = fetchurl {
        url = "https://registry.npmjs.org/@nodelib/fs.walk/-/fs.walk-1.2.7.tgz";
        sha512 = "BTIhocbPBSrRmHxOAJFtR18oLhxTtAFDAvL8hY1S3iU8k+E60W/YFs4jrixGzQjMpF4qPXxIQHcjVD9dz1C2QA==";
      };
    };
    "ansi-regex-5.0.0" = {
      name = "ansi-regex";
      packageName = "ansi-regex";
      version = "5.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-5.0.0.tgz";
        sha512 = "bY6fj56OUQ0hU1KjFNDQuJFezqKdrAyFdIevADiqrWHwSlbmBNMHp5ak2f40Pm8JTFyM2mqxkG6ngkHO11f/lg==";
      };
    };
    "ansi-styles-4.3.0" = {
      name = "ansi-styles";
      packageName = "ansi-styles";
      version = "4.3.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/ansi-styles/-/ansi-styles-4.3.0.tgz";
        sha512 = "zbB9rCJAT1rbjiVDb2hqKFHNYLxgtk8NURxZ3IZwD3F6NtxbXZQCnnSi1Lkx+IDohdPlFp222wVALIheZJQSEg==";
      };
    };
    "anymatch-3.1.2" = {
      name = "anymatch";
      packageName = "anymatch";
      version = "3.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/anymatch/-/anymatch-3.1.2.tgz";
        sha512 = "P43ePfOAIupkguHUycrc4qJ9kz8ZiuOUijaETwX7THt0Y/GNK7v0aa8rY816xWjZ7rJdA5XdMcpVFTKMq+RvWg==";
      };
    };
    "array-union-2.1.0" = {
      name = "array-union";
      packageName = "array-union";
      version = "2.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/array-union/-/array-union-2.1.0.tgz";
        sha512 = "HGyxoOTYUyCM6stUe6EJgnd4EoewAI7zMdfqO+kGjnlZmBDz/cR5pf8r/cR4Wq60sL/p0IkcjUEEPwS3GFrIyw==";
      };
    };
    "at-least-node-1.0.0" = {
      name = "at-least-node";
      packageName = "at-least-node";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/at-least-node/-/at-least-node-1.0.0.tgz";
        sha512 = "+q/t7Ekv1EDY2l6Gda6LLiX14rU9TV20Wa3ofeQmwPFZbOMo9DXrLbOjFaaclkXKWidIaopwAObQDqwWtGUjqg==";
      };
    };
    "autoprefixer-10.2.6" = {
      name = "autoprefixer";
      packageName = "autoprefixer";
      version = "10.2.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/autoprefixer/-/autoprefixer-10.2.6.tgz";
        sha512 = "8lChSmdU6dCNMCQopIf4Pe5kipkAGj/fvTMslCsih0uHpOrXOPUEVOmYMMqmw3cekQkSD7EhIeuYl5y0BLdKqg==";
      };
    };
    "binary-extensions-2.2.0" = {
      name = "binary-extensions";
      packageName = "binary-extensions";
      version = "2.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/binary-extensions/-/binary-extensions-2.2.0.tgz";
        sha512 = "jDctJ/IVQbZoJykoeHbhXpOlNBqGNcwXJKJog42E5HDPUwQTSdjCHdihjj0DlnheQ7blbT6dHOafNAiS8ooQKA==";
      };
    };
    "braces-3.0.2" = {
      name = "braces";
      packageName = "braces";
      version = "3.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/braces/-/braces-3.0.2.tgz";
        sha512 = "b8um+L1RzM3WDSzvhm6gIz1yfTbBt6YTlcEKAvsmqCZZFw46z626lVj9j1yEPW33H5H+lBQpZMP1k8l+78Ha0A==";
      };
    };
    "browserslist-4.16.6" = {
      name = "browserslist";
      packageName = "browserslist";
      version = "4.16.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/browserslist/-/browserslist-4.16.6.tgz";
        sha512 = "Wspk/PqO+4W9qp5iUTJsa1B/QrYn1keNCcEP5OvP7WBwT4KaDly0uONYmC6Xa3Z5IqnUgS0KcgLYu1l74x0ZXQ==";
      };
    };
    "caniuse-lite-1.0.30001242" = {
      name = "caniuse-lite";
      packageName = "caniuse-lite";
      version = "1.0.30001242";
      src = fetchurl {
        url = "https://registry.npmjs.org/caniuse-lite/-/caniuse-lite-1.0.30001242.tgz";
        sha512 = "KvNuZ/duufelMB3w2xtf9gEWCSxJwUgoxOx5b6ScLXC4kPc9xsczUVCPrQU26j5kOsHM4pSUL54tAZt5THQKug==";
      };
    };
    "chalk-4.1.1" = {
      name = "chalk";
      packageName = "chalk";
      version = "4.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/chalk/-/chalk-4.1.1.tgz";
        sha512 = "diHzdDKxcU+bAsUboHLPEDQiw0qEe0qd7SYUn3HgcFlWgbDcfLGswOHYeGrHKzG9z6UYf01d9VFMfZxPM1xZSg==";
      };
    };
    "chokidar-3.5.2" = {
      name = "chokidar";
      packageName = "chokidar";
      version = "3.5.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/chokidar/-/chokidar-3.5.2.tgz";
        sha512 = "ekGhOnNVPgT77r4K/U3GDhu+FQ2S8TnK/s2KbIGXi0SZWuwkZ2QNyfWdZW+TVfn84DpEP7rLeCt2UI6bJ8GwbQ==";
      };
    };
    "cliui-7.0.4" = {
      name = "cliui";
      packageName = "cliui";
      version = "7.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/cliui/-/cliui-7.0.4.tgz";
        sha512 = "OcRE68cOsVMXp1Yvonl/fzkQOyjLSu/8bhPDfQt0e0/Eb283TKP20Fs2MqoPsr9SwA595rRCA+QMzYc9nBP+JQ==";
      };
    };
    "color-convert-2.0.1" = {
      name = "color-convert";
      packageName = "color-convert";
      version = "2.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/color-convert/-/color-convert-2.0.1.tgz";
        sha512 = "RRECPsj7iu/xb5oKYcsFHSppFNnsj/52OVTRKb4zP5onXwVF3zVmmToNcOfGC+CRDpfK/U584fMg38ZHCaElKQ==";
      };
    };
    "color-name-1.1.4" = {
      name = "color-name";
      packageName = "color-name";
      version = "1.1.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/color-name/-/color-name-1.1.4.tgz";
        sha512 = "dOy+3AuW3a2wNbZHIuMZpTcgjGuLU/uBL/ubcZF9OXbDo8ff4O8yVp5Bf0efS8uEoYo5q4Fx7dY9OgQGXgAsQA==";
      };
    };
    "colorette-1.2.2" = {
      name = "colorette";
      packageName = "colorette";
      version = "1.2.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/colorette/-/colorette-1.2.2.tgz";
        sha512 = "MKGMzyfeuutC/ZJ1cba9NqcNpfeqMUcYmyF1ZFY6/Cn7CNSAKx6a+s48sqLqyAiZuaP2TcqMhoo+dlwFnVxT9w==";
      };
    };
    "dependency-graph-0.9.0" = {
      name = "dependency-graph";
      packageName = "dependency-graph";
      version = "0.9.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/dependency-graph/-/dependency-graph-0.9.0.tgz";
        sha512 = "9YLIBURXj4DJMFALxXw9K3Y3rwb5Fk0X5/8ipCzaN84+gKxoHK43tVKRNakCQbiEx07E8Uwhuq21BpUagFhZ8w==";
      };
    };
    "dir-glob-3.0.1" = {
      name = "dir-glob";
      packageName = "dir-glob";
      version = "3.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/dir-glob/-/dir-glob-3.0.1.tgz";
        sha512 = "WkrWp9GR4KXfKGYzOLmTuGVi1UWFfws377n9cc55/tb6DuqyF6pcQ5AbiHEshaDpY9v6oaSr2XCDidGmMwdzIA==";
      };
    };
    "electron-to-chromium-1.3.766" = {
      name = "electron-to-chromium";
      packageName = "electron-to-chromium";
      version = "1.3.766";
      src = fetchurl {
        url = "https://registry.npmjs.org/electron-to-chromium/-/electron-to-chromium-1.3.766.tgz";
        sha512 = "u2quJ862q9reRKh/je3GXis3w38+RoXH1J9N3XjtsS6NzmUAosNsyZgUVFZPN/ZlJ3v6T0rTyZR3q/J5c6Sy5w==";
      };
    };
    "emoji-regex-8.0.0" = {
      name = "emoji-regex";
      packageName = "emoji-regex";
      version = "8.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/emoji-regex/-/emoji-regex-8.0.0.tgz";
        sha512 = "MSjYzcWNOA0ewAHpz0MxpYFvwg6yjy1NG3xteoqz644VCo/RPgnr1/GGt+ic3iJTzQ8Eu3TdM14SawnVUmGE6A==";
      };
    };
    "escalade-3.1.1" = {
      name = "escalade";
      packageName = "escalade";
      version = "3.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/escalade/-/escalade-3.1.1.tgz";
        sha512 = "k0er2gUkLf8O0zKJiAhmkTnJlTvINGv7ygDNPbeIsX/TJjGJZHuh9B2UxbsaEkmlEo9MfhrSzmhIlhRlI2GXnw==";
      };
    };
    "fast-glob-3.2.6" = {
      name = "fast-glob";
      packageName = "fast-glob";
      version = "3.2.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/fast-glob/-/fast-glob-3.2.6.tgz";
        sha512 = "GnLuqj/pvQ7pX8/L4J84nijv6sAnlwvSDpMkJi9i7nPmPxGtRPkBSStfvDW5l6nMdX9VWe+pkKWFTgD+vF2QSQ==";
      };
    };
    "fastq-1.11.1" = {
      name = "fastq";
      packageName = "fastq";
      version = "1.11.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/fastq/-/fastq-1.11.1.tgz";
        sha512 = "HOnr8Mc60eNYl1gzwp6r5RoUyAn5/glBolUzP/Ez6IFVPMPirxn/9phgL6zhOtaTy7ISwPvQ+wT+hfcRZh/bzw==";
      };
    };
    "fill-range-7.0.1" = {
      name = "fill-range";
      packageName = "fill-range";
      version = "7.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/fill-range/-/fill-range-7.0.1.tgz";
        sha512 = "qOo9F+dMUmC2Lcb4BbVvnKJxTPjCm+RRpe4gDuGrzkL7mEVl/djYSu2OdQ2Pa302N4oqkSg9ir6jaLWJ2USVpQ==";
      };
    };
    "fraction.js-4.1.1" = {
      name = "fraction.js";
      packageName = "fraction.js";
      version = "4.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/fraction.js/-/fraction.js-4.1.1.tgz";
        sha512 = "MHOhvvxHTfRFpF1geTK9czMIZ6xclsEor2wkIGYYq+PxcQqT7vStJqjhe6S1TenZrMZzo+wlqOufBDVepUEgPg==";
      };
    };
    "fs-extra-9.1.0" = {
      name = "fs-extra";
      packageName = "fs-extra";
      version = "9.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/fs-extra/-/fs-extra-9.1.0.tgz";
        sha512 = "hcg3ZmepS30/7BSFqRvoo3DOMQu7IjqxO5nCDt+zM9XWjb33Wg7ziNT+Qvqbuc3+gWpzO02JubVyk2G4Zvo1OQ==";
      };
    };
    "fsevents-2.3.2" = {
      name = "fsevents";
      packageName = "fsevents";
      version = "2.3.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/fsevents/-/fsevents-2.3.2.tgz";
        sha512 = "xiqMQR4xAeHTuB9uWm+fFRcIOgKBMiOBP+eXiyT7jsgVCq1bkVygt00oASowB7EdtpOHaaPgKt812P9ab+DDKA==";
      };
    };
    "get-caller-file-2.0.5" = {
      name = "get-caller-file";
      packageName = "get-caller-file";
      version = "2.0.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/get-caller-file/-/get-caller-file-2.0.5.tgz";
        sha512 = "DyFP3BM/3YHTQOCUL/w0OZHR0lpKeGrxotcHWcqNEdnltqFwXVfhEBQ94eIo34AfQpo0rGki4cyIiftY06h2Fg==";
      };
    };
    "get-stdin-8.0.0" = {
      name = "get-stdin";
      packageName = "get-stdin";
      version = "8.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/get-stdin/-/get-stdin-8.0.0.tgz";
        sha512 = "sY22aA6xchAzprjyqmSEQv4UbAAzRN0L2dQB0NlN5acTTK9Don6nhoc3eAbUnpZiCANAMfd/+40kVdKfFygohg==";
      };
    };
    "glob-parent-5.1.2" = {
      name = "glob-parent";
      packageName = "glob-parent";
      version = "5.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/glob-parent/-/glob-parent-5.1.2.tgz";
        sha512 = "AOIgSQCepiJYwP3ARnGx+5VnTu2HBYdzbGP45eLw1vr3zB3vZLeyed1sC9hnbcOc9/SrMyM5RPQrkGz4aS9Zow==";
      };
    };
    "globby-11.0.4" = {
      name = "globby";
      packageName = "globby";
      version = "11.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/globby/-/globby-11.0.4.tgz";
        sha512 = "9O4MVG9ioZJ08ffbcyVYyLOJLk5JQ688pJ4eMGLpdWLHq/Wr1D9BlriLQyL0E+jbkuePVZXYFj47QM/v093wHg==";
      };
    };
    "graceful-fs-4.2.6" = {
      name = "graceful-fs";
      packageName = "graceful-fs";
      version = "4.2.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/graceful-fs/-/graceful-fs-4.2.6.tgz";
        sha512 = "nTnJ528pbqxYanhpDYsi4Rd8MAeaBA67+RZ10CM1m3bTAVFEDcd5AuA4a6W5YkGZ1iNXHzZz8T6TBKLeBuNriQ==";
      };
    };
    "has-flag-4.0.0" = {
      name = "has-flag";
      packageName = "has-flag";
      version = "4.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/has-flag/-/has-flag-4.0.0.tgz";
        sha512 = "EykJT/Q1KjTWctppgIAgfSO0tKVuZUjhgMr17kqTumMl6Afv3EISleU7qZUzoXDFTAHTDC4NOoG/ZxU3EvlMPQ==";
      };
    };
    "ignore-5.1.8" = {
      name = "ignore";
      packageName = "ignore";
      version = "5.1.8";
      src = fetchurl {
        url = "https://registry.npmjs.org/ignore/-/ignore-5.1.8.tgz";
        sha512 = "BMpfD7PpiETpBl/A6S498BaIJ6Y/ABT93ETbby2fP00v4EbvPBXWEoaR1UBPKs3iR53pJY7EtZk5KACI57i1Uw==";
      };
    };
    "import-cwd-3.0.0" = {
      name = "import-cwd";
      packageName = "import-cwd";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/import-cwd/-/import-cwd-3.0.0.tgz";
        sha512 = "4pnzH16plW+hgvRECbDWpQl3cqtvSofHWh44met7ESfZ8UZOWWddm8hEyDTqREJ9RbYHY8gi8DqmaelApoOGMg==";
      };
    };
    "import-from-3.0.0" = {
      name = "import-from";
      packageName = "import-from";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/import-from/-/import-from-3.0.0.tgz";
        sha512 = "CiuXOFFSzkU5x/CR0+z7T91Iht4CXgfCxVOFRhh2Zyhg5wOpWvvDLQUsWl+gcN+QscYBjez8hDCt85O7RLDttQ==";
      };
    };
    "is-binary-path-2.1.0" = {
      name = "is-binary-path";
      packageName = "is-binary-path";
      version = "2.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/is-binary-path/-/is-binary-path-2.1.0.tgz";
        sha512 = "ZMERYes6pDydyuGidse7OsHxtbI7WVeUEozgR/g7rd0xUimYNlvZRE/K2MgZTjWy725IfelLeVcEM97mmtRGXw==";
      };
    };
    "is-extglob-2.1.1" = {
      name = "is-extglob";
      packageName = "is-extglob";
      version = "2.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/is-extglob/-/is-extglob-2.1.1.tgz";
        sha1 = "a88c02535791f02ed37c76a1b9ea9773c833f8c2";
      };
    };
    "is-fullwidth-code-point-3.0.0" = {
      name = "is-fullwidth-code-point";
      packageName = "is-fullwidth-code-point";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-3.0.0.tgz";
        sha512 = "zymm5+u+sCsSWyD9qNaejV3DFvhCKclKdizYaJUuHA83RLjb7nSuGnddCHGv0hk+KY7BMAlsWeK4Ueg6EV6XQg==";
      };
    };
    "is-glob-4.0.1" = {
      name = "is-glob";
      packageName = "is-glob";
      version = "4.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/is-glob/-/is-glob-4.0.1.tgz";
        sha512 = "5G0tKtBTFImOqDnLB2hG6Bp2qcKEFduo4tZu9MT/H6NQv/ghhy30o55ufafxJ/LdH79LLs2Kfrn85TLKyA7BUg==";
      };
    };
    "is-number-7.0.0" = {
      name = "is-number";
      packageName = "is-number";
      version = "7.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/is-number/-/is-number-7.0.0.tgz";
        sha512 = "41Cifkg6e8TylSpdtTpeLVMqvSBEVzTttHvERD741+pnZ8ANv0004MRL43QKPDlK9cGvNp6NZWZUBlbGXYxxng==";
      };
    };
    "jsonfile-6.1.0" = {
      name = "jsonfile";
      packageName = "jsonfile";
      version = "6.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/jsonfile/-/jsonfile-6.1.0.tgz";
        sha512 = "5dgndWOriYSm5cnYaJNhalLNDKOqFwyDB/rr1E9ZsGciGvKPs8R2xYGCacuf3z6K1YKDz182fd+fY3cn3pMqXQ==";
      };
    };
    "lilconfig-2.0.3" = {
      name = "lilconfig";
      packageName = "lilconfig";
      version = "2.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/lilconfig/-/lilconfig-2.0.3.tgz";
        sha512 = "EHKqr/+ZvdKCifpNrJCKxBTgk5XupZA3y/aCPY9mxfgBzmgh93Mt/WqjjQ38oMxXuvDokaKiM3lAgvSH2sjtHg==";
      };
    };
    "lodash.difference-4.5.0" = {
      name = "lodash.difference";
      packageName = "lodash.difference";
      version = "4.5.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/lodash.difference/-/lodash.difference-4.5.0.tgz";
        sha1 = "9ccb4e505d486b91651345772885a2df27fd017c";
      };
    };
    "lodash.forown-4.4.0" = {
      name = "lodash.forown";
      packageName = "lodash.forown";
      version = "4.4.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/lodash.forown/-/lodash.forown-4.4.0.tgz";
        sha1 = "85115cf04f73ef966eced52511d3893cc46683af";
      };
    };
    "lodash.get-4.4.2" = {
      name = "lodash.get";
      packageName = "lodash.get";
      version = "4.4.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/lodash.get/-/lodash.get-4.4.2.tgz";
        sha1 = "2d177f652fa31e939b4438d5341499dfa3825e99";
      };
    };
    "lodash.groupby-4.6.0" = {
      name = "lodash.groupby";
      packageName = "lodash.groupby";
      version = "4.6.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/lodash.groupby/-/lodash.groupby-4.6.0.tgz";
        sha1 = "0b08a1dcf68397c397855c3239783832df7403d1";
      };
    };
    "lodash.sortby-4.7.0" = {
      name = "lodash.sortby";
      packageName = "lodash.sortby";
      version = "4.7.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/lodash.sortby/-/lodash.sortby-4.7.0.tgz";
        sha1 = "edd14c824e2cc9c1e0b0a1b42bb5210516a42438";
      };
    };
    "merge2-1.4.1" = {
      name = "merge2";
      packageName = "merge2";
      version = "1.4.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/merge2/-/merge2-1.4.1.tgz";
        sha512 = "8q7VEgMJW4J8tcfVPy8g09NcQwZdbwFEqhe/WZkoIzjn/3TGDwtOCYtXGxA3O8tPzpczCCDgv+P2P5y00ZJOOg==";
      };
    };
    "micromatch-4.0.4" = {
      name = "micromatch";
      packageName = "micromatch";
      version = "4.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/micromatch/-/micromatch-4.0.4.tgz";
        sha512 = "pRmzw/XUcwXGpD9aI9q/0XOwLNygjETJ8y0ao0wdqprrzDa4YnxLcz7fQRZr8voh8V10kGhABbNcHVk5wHgWwg==";
      };
    };
    "nanoid-3.1.23" = {
      name = "nanoid";
      packageName = "nanoid";
      version = "3.1.23";
      src = fetchurl {
        url = "https://registry.npmjs.org/nanoid/-/nanoid-3.1.23.tgz";
        sha512 = "FiB0kzdP0FFVGDKlRLEQ1BgDzU87dy5NnzjeW9YZNt+/c3+q82EQDUwniSAUxp/F0gFNI1ZhKU1FqYsMuqZVnw==";
      };
    };
    "node-releases-1.1.73" = {
      name = "node-releases";
      packageName = "node-releases";
      version = "1.1.73";
      src = fetchurl {
        url = "https://registry.npmjs.org/node-releases/-/node-releases-1.1.73.tgz";
        sha512 = "uW7fodD6pyW2FZNZnp/Z3hvWKeEW1Y8R1+1CnErE8cXFXzl5blBOoVB41CvMer6P6Q0S5FXDwcHgFd1Wj0U9zg==";
      };
    };
    "normalize-path-3.0.0" = {
      name = "normalize-path";
      packageName = "normalize-path";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/normalize-path/-/normalize-path-3.0.0.tgz";
        sha512 = "6eZs5Ls3WtCisHWp9S2GUy8dqkpGi4BVSz3GaqiE6ezub0512ESztXUwUB6C6IKbQkY2Pnb/mD4WYojCRwcwLA==";
      };
    };
    "normalize-range-0.1.2" = {
      name = "normalize-range";
      packageName = "normalize-range";
      version = "0.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/normalize-range/-/normalize-range-0.1.2.tgz";
        sha1 = "2d10c06bdfd312ea9777695a4d28439456b75942";
      };
    };
    "path-type-4.0.0" = {
      name = "path-type";
      packageName = "path-type";
      version = "4.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/path-type/-/path-type-4.0.0.tgz";
        sha512 = "gDKb8aZMDeD/tZWs9P6+q0J9Mwkdl6xMV8TjnGP3qJVJ06bdMgkbBlLU8IdfOsIsFz2BW1rNVT3XuNEl8zPAvw==";
      };
    };
    "picomatch-2.3.0" = {
      name = "picomatch";
      packageName = "picomatch";
      version = "2.3.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/picomatch/-/picomatch-2.3.0.tgz";
        sha512 = "lY1Q/PiJGC2zOv/z391WOTD+Z02bCgsFfvxoXXf6h7kv9o+WmsmzYqrAwY63sNgOxE4xEdq0WyUnXfKeBrSvYw==";
      };
    };
    "pify-2.3.0" = {
      name = "pify";
      packageName = "pify";
      version = "2.3.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/pify/-/pify-2.3.0.tgz";
        sha1 = "ed141a6ac043a849ea588498e7dca8b15330e90c";
      };
    };
    "postcss-8.3.5" = {
      name = "postcss";
      packageName = "postcss";
      version = "8.3.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/postcss/-/postcss-8.3.5.tgz";
        sha512 = "NxTuJocUhYGsMiMFHDUkmjSKT3EdH4/WbGF6GCi1NDGk+vbcUTun4fpbOqaPtD8IIsztA2ilZm2DhYCuyN58gA==";
      };
    };
    "postcss-cli-8.3.1" = {
      name = "postcss-cli";
      packageName = "postcss-cli";
      version = "8.3.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/postcss-cli/-/postcss-cli-8.3.1.tgz";
        sha512 = "leHXsQRq89S3JC9zw/tKyiVV2jAhnfQe0J8VI4eQQbUjwIe0XxVqLrR+7UsahF1s9wi4GlqP6SJ8ydf44cgF2Q==";
      };
    };
    "postcss-load-config-3.1.0" = {
      name = "postcss-load-config";
      packageName = "postcss-load-config";
      version = "3.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/postcss-load-config/-/postcss-load-config-3.1.0.tgz";
        sha512 = "ipM8Ds01ZUophjDTQYSVP70slFSYg3T0/zyfII5vzhN6V57YSxMgG5syXuwi5VtS8wSf3iL30v0uBdoIVx4Q0g==";
      };
    };
    "postcss-reporter-7.0.2" = {
      name = "postcss-reporter";
      packageName = "postcss-reporter";
      version = "7.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/postcss-reporter/-/postcss-reporter-7.0.2.tgz";
        sha512 = "JyQ96NTQQsso42y6L1H1RqHfWH1C3Jr0pt91mVv5IdYddZAE9DUZxuferNgk6q0o6vBVOrfVJb10X1FgDzjmDw==";
      };
    };
    "postcss-value-parser-4.1.0" = {
      name = "postcss-value-parser";
      packageName = "postcss-value-parser";
      version = "4.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/postcss-value-parser/-/postcss-value-parser-4.1.0.tgz";
        sha512 = "97DXOFbQJhk71ne5/Mt6cOu6yxsSfM0QGQyl0L25Gca4yGWEGJaig7l7gbCX623VqTBNGLRLaVUCnNkcedlRSQ==";
      };
    };
    "pretty-hrtime-1.0.3" = {
      name = "pretty-hrtime";
      packageName = "pretty-hrtime";
      version = "1.0.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/pretty-hrtime/-/pretty-hrtime-1.0.3.tgz";
        sha1 = "b7e3ea42435a4c9b2759d99e0f201eb195802ee1";
      };
    };
    "queue-microtask-1.2.3" = {
      name = "queue-microtask";
      packageName = "queue-microtask";
      version = "1.2.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/queue-microtask/-/queue-microtask-1.2.3.tgz";
        sha512 = "NuaNSa6flKT5JaSYQzJok04JzTL1CA6aGhv5rfLW3PgqA+M2ChpZQnAC8h8i4ZFkBS8X5RqkDBHA7r4hej3K9A==";
      };
    };
    "read-cache-1.0.0" = {
      name = "read-cache";
      packageName = "read-cache";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/read-cache/-/read-cache-1.0.0.tgz";
        sha1 = "e664ef31161166c9751cdbe8dbcf86b5fb58f774";
      };
    };
    "readdirp-3.6.0" = {
      name = "readdirp";
      packageName = "readdirp";
      version = "3.6.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/readdirp/-/readdirp-3.6.0.tgz";
        sha512 = "hOS089on8RduqdbhvQ5Z37A0ESjsqz6qnRcffsMU3495FuTdqSm+7bhJ29JvIOsBDEEnan5DPu9t3To9VRlMzA==";
      };
    };
    "require-directory-2.1.1" = {
      name = "require-directory";
      packageName = "require-directory";
      version = "2.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/require-directory/-/require-directory-2.1.1.tgz";
        sha1 = "8c64ad5fd30dab1c976e2344ffe7f792a6a6df42";
      };
    };
    "resolve-from-5.0.0" = {
      name = "resolve-from";
      packageName = "resolve-from";
      version = "5.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/resolve-from/-/resolve-from-5.0.0.tgz";
        sha512 = "qYg9KP24dD5qka9J47d0aVky0N+b4fTU89LN9iDnjB5waksiC49rvMB0PrUJQGoTmH50XPiqOvAjDfaijGxYZw==";
      };
    };
    "reusify-1.0.4" = {
      name = "reusify";
      packageName = "reusify";
      version = "1.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/reusify/-/reusify-1.0.4.tgz";
        sha512 = "U9nH88a3fc/ekCF1l0/UP1IosiuIjyTh7hBvXVMHYgVcfGvt897Xguj2UOLDeI5BG2m7/uwyaLVT6fbtCwTyzw==";
      };
    };
    "run-parallel-1.2.0" = {
      name = "run-parallel";
      packageName = "run-parallel";
      version = "1.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/run-parallel/-/run-parallel-1.2.0.tgz";
        sha512 = "5l4VyZR86LZ/lDxZTR6jqL8AFE2S0IFLMP26AbjsLVADxHdhB/c0GUsH+y39UfCi3dzz8OlQuPmnaJOMoDHQBA==";
      };
    };
    "slash-3.0.0" = {
      name = "slash";
      packageName = "slash";
      version = "3.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/slash/-/slash-3.0.0.tgz";
        sha512 = "g9Q1haeby36OSStwb4ntCGGGaKsaVSjQ68fBxoQcutl5fS1vuY18H3wSt3jFyFtrkx+Kz0V1G85A4MyAdDMi2Q==";
      };
    };
    "source-map-js-0.6.2" = {
      name = "source-map-js";
      packageName = "source-map-js";
      version = "0.6.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/source-map-js/-/source-map-js-0.6.2.tgz";
        sha512 = "/3GptzWzu0+0MBQFrDKzw/DvvMTUORvgY6k6jd/VS6iCR4RDTKWH6v6WPwQoUO8667uQEf9Oe38DxAYWY5F/Ug==";
      };
    };
    "string-width-4.2.2" = {
      name = "string-width";
      packageName = "string-width";
      version = "4.2.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/string-width/-/string-width-4.2.2.tgz";
        sha512 = "XBJbT3N4JhVumXE0eoLU9DCjcaF92KLNqTmFCnG1pf8duUxFGwtP6AD6nkjw9a3IdiRtL3E2w3JDiE/xi3vOeA==";
      };
    };
    "strip-ansi-6.0.0" = {
      name = "strip-ansi";
      packageName = "strip-ansi";
      version = "6.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-6.0.0.tgz";
        sha512 = "AuvKTrTfQNYNIctbR1K/YGTR1756GycPsg7b9bdV9Duqur4gv6aKqHXah67Z8ImS7WEz5QVcOtlfW2rZEugt6w==";
      };
    };
    "supports-color-7.2.0" = {
      name = "supports-color";
      packageName = "supports-color";
      version = "7.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/supports-color/-/supports-color-7.2.0.tgz";
        sha512 = "qpCAvRl9stuOHveKsn7HncJRvv501qIacKzQlO/+Lwxc9+0q2wLyv4Dfvt80/DPn2pqOBsJdDiogXGR9+OvwRw==";
      };
    };
    "to-regex-range-5.0.1" = {
      name = "to-regex-range";
      packageName = "to-regex-range";
      version = "5.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/to-regex-range/-/to-regex-range-5.0.1.tgz";
        sha512 = "65P7iz6X5yEr1cwcgvQxbbIw7Uk3gOy5dIdtZ4rDveLqhrdJP+Li/Hx6tyK0NEb+2GCyneCMJiGqrADCSNk8sQ==";
      };
    };
    "universalify-2.0.0" = {
      name = "universalify";
      packageName = "universalify";
      version = "2.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/universalify/-/universalify-2.0.0.tgz";
        sha512 = "hAZsKq7Yy11Zu1DE0OzWjw7nnLZmJZYTDZZyEFHZdUhV8FkH5MCfoU1XMaxXovpyW5nq5scPqq0ZDP9Zyl04oQ==";
      };
    };
    "wrap-ansi-7.0.0" = {
      name = "wrap-ansi";
      packageName = "wrap-ansi";
      version = "7.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/wrap-ansi/-/wrap-ansi-7.0.0.tgz";
        sha512 = "YVGIj2kamLSTxw6NsZjoBxfSwsn0ycdesmc4p+Q21c5zPuZ1pl+NfxVdxPtdHvmNVOQ6XSYG4AUtyt/Fi7D16Q==";
      };
    };
    "y18n-5.0.8" = {
      name = "y18n";
      packageName = "y18n";
      version = "5.0.8";
      src = fetchurl {
        url = "https://registry.npmjs.org/y18n/-/y18n-5.0.8.tgz";
        sha512 = "0pfFzegeDWJHJIAmTLRP2DwHjdF5s7jo9tuztdQxAhINCdvS+3nGINqPd00AphqJR/0LhANUS6/+7SCb98YOfA==";
      };
    };
    "yaml-1.10.2" = {
      name = "yaml";
      packageName = "yaml";
      version = "1.10.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/yaml/-/yaml-1.10.2.tgz";
        sha512 = "r3vXyErRCYJ7wg28yvBY5VSoAF8ZvlcW9/BwUzEtUsjvX/DKs24dIkuwjtuprwJJHsbyUbLApepYTR1BN4uHrg==";
      };
    };
    "yargs-16.2.0" = {
      name = "yargs";
      packageName = "yargs";
      version = "16.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/yargs/-/yargs-16.2.0.tgz";
        sha512 = "D1mvvtDG0L5ft/jGWkLpG1+m0eQxOfaBvTNELraWj22wSVUMWxZUvYgJYcKh6jGGIkJFhH4IZPQhR4TKpc8mBw==";
      };
    };
    "yargs-parser-20.2.9" = {
      name = "yargs-parser";
      packageName = "yargs-parser";
      version = "20.2.9";
      src = fetchurl {
        url = "https://registry.npmjs.org/yargs-parser/-/yargs-parser-20.2.9.tgz";
        sha512 = "y11nGElTIV+CT3Zv9t7VKl+Q3hTQoT9a1Qzezhhl6Rp21gJ/IVTW7Z3y9EWXhuUBC2Shnf+DX0antecpAwSP8w==";
      };
    };
  };
  args = {
    name = "client";
    packageName = "client";
    version = "1.0.0";
    src = ./.;
    dependencies = [
      sources."@nodelib/fs.scandir-2.1.5"
      sources."@nodelib/fs.stat-2.0.5"
      sources."@nodelib/fs.walk-1.2.7"
      sources."ansi-regex-5.0.0"
      sources."ansi-styles-4.3.0"
      sources."anymatch-3.1.2"
      sources."array-union-2.1.0"
      sources."at-least-node-1.0.0"
      sources."autoprefixer-10.2.6"
      sources."binary-extensions-2.2.0"
      sources."braces-3.0.2"
      sources."browserslist-4.16.6"
      sources."caniuse-lite-1.0.30001242"
      sources."chalk-4.1.1"
      sources."chokidar-3.5.2"
      sources."cliui-7.0.4"
      sources."color-convert-2.0.1"
      sources."color-name-1.1.4"
      sources."colorette-1.2.2"
      sources."dependency-graph-0.9.0"
      sources."dir-glob-3.0.1"
      sources."electron-to-chromium-1.3.766"
      sources."emoji-regex-8.0.0"
      sources."escalade-3.1.1"
      sources."fast-glob-3.2.6"
      sources."fastq-1.11.1"
      sources."fill-range-7.0.1"
      sources."fraction.js-4.1.1"
      sources."fs-extra-9.1.0"
      sources."fsevents-2.3.2"
      sources."get-caller-file-2.0.5"
      sources."get-stdin-8.0.0"
      sources."glob-parent-5.1.2"
      sources."globby-11.0.4"
      sources."graceful-fs-4.2.6"
      sources."has-flag-4.0.0"
      sources."ignore-5.1.8"
      sources."import-cwd-3.0.0"
      sources."import-from-3.0.0"
      sources."is-binary-path-2.1.0"
      sources."is-extglob-2.1.1"
      sources."is-fullwidth-code-point-3.0.0"
      sources."is-glob-4.0.1"
      sources."is-number-7.0.0"
      sources."jsonfile-6.1.0"
      sources."lilconfig-2.0.3"
      sources."lodash.difference-4.5.0"
      sources."lodash.forown-4.4.0"
      sources."lodash.get-4.4.2"
      sources."lodash.groupby-4.6.0"
      sources."lodash.sortby-4.7.0"
      sources."merge2-1.4.1"
      sources."micromatch-4.0.4"
      sources."nanoid-3.1.23"
      sources."node-releases-1.1.73"
      sources."normalize-path-3.0.0"
      sources."normalize-range-0.1.2"
      sources."path-type-4.0.0"
      sources."picomatch-2.3.0"
      sources."pify-2.3.0"
      sources."postcss-8.3.5"
      sources."postcss-cli-8.3.1"
      sources."postcss-load-config-3.1.0"
      sources."postcss-reporter-7.0.2"
      sources."postcss-value-parser-4.1.0"
      sources."pretty-hrtime-1.0.3"
      sources."queue-microtask-1.2.3"
      sources."read-cache-1.0.0"
      sources."readdirp-3.6.0"
      sources."require-directory-2.1.1"
      sources."resolve-from-5.0.0"
      sources."reusify-1.0.4"
      sources."run-parallel-1.2.0"
      sources."slash-3.0.0"
      sources."source-map-js-0.6.2"
      sources."string-width-4.2.2"
      sources."strip-ansi-6.0.0"
      sources."supports-color-7.2.0"
      sources."to-regex-range-5.0.1"
      sources."universalify-2.0.0"
      sources."wrap-ansi-7.0.0"
      sources."y18n-5.0.8"
      sources."yaml-1.10.2"
      sources."yargs-16.2.0"
      sources."yargs-parser-20.2.9"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "";
      license = "ISC";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
in
{
  args = args;
  sources = sources;
  tarball = nodeEnv.buildNodeSourceDist args;
  package = nodeEnv.buildNodePackage args;
  shell = nodeEnv.buildNodeShell args;
  nodeDependencies = nodeEnv.buildNodeDependencies (lib.overrideExisting args {
    src = stdenv.mkDerivation {
      name = args.name + "-package-json";
      src = nix-gitignore.gitignoreSourcePure [
        "*"
        "!package.json"
        "!package-lock.json"
      ] args.src;
      dontBuild = true;
      installPhase = "mkdir -p $out; cp -r ./* $out;";
    };
  });
}
