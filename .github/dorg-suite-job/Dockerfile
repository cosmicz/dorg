FROM debian:bookworm-slim@sha256:88200866dfff7ea7f5cbcb6ec7c8a701889efe6fe859fe64d6990e4b07ea4171

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
ARG TARGETARCH
ARG EMACS_VERSION
ARG EMACS_SHA256

RUN [[ -n "$EMACS_VERSION" && "$EMACS_SHA256" =~ ^[0-9a-f]{64}$ ]]

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates curl xz-utils tar gzip git lsof \
      build-essential make gcc libc6-dev \
      pkg-config texinfo \
      libgnutls28-dev zlib1g-dev libxml2-dev libncurses-dev && \
    rm -rf /var/lib/apt/lists/*

RUN set -euo pipefail; \
    cd /tmp; \
    curl -sSL -o "emacs-${EMACS_VERSION}.tar.xz" "https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VERSION}.tar.xz"; \
    echo "${EMACS_SHA256}  emacs-${EMACS_VERSION}.tar.xz" | sha256sum -c -; \
    tar xJf "emacs-${EMACS_VERSION}.tar.xz"; \
    cd "emacs-${EMACS_VERSION}"; \
    ./configure \
      --without-x \
      --without-sound \
      --without-pop \
      --without-mailutils \
      --without-dbus \
      --without-gsettings \
      --without-selinux \
      --without-xwidgets \
      --without-native-compilation \
      --with-gnutls \
      --with-zlib \
      --with-xml2; \
    make -j"$(nproc)"; \
    make install; \
    cd /tmp; \
    rm -rf "emacs-${EMACS_VERSION}" "emacs-${EMACS_VERSION}.tar.xz"; \
    emacs --version | head -n 1

# Dolt 1.86.4 is selected only from BuildKit's declared target architecture.
# Stock v1.86.1 panics in isNoopUpdate on the headings UPDATE (VARBINARY
# index + foreign key; upstream PR 10906, released in v1.86.4).
# The identity lines from the old host step intentionally move to the runtime
# entrypoint so they are written under the arbitrary uid's job-private HOME.
RUN set -euo pipefail; \
    cd /tmp; \
    case "$TARGETARCH" in \
      amd64) dolt_archive=dolt-linux-amd64.tar.gz; dolt_sha256=88bdca99086bbac7f2414af2756c962d20a8d9b6463861dc2cf35111f13b6dea ;; \
      arm64) dolt_archive=dolt-linux-arm64.tar.gz; dolt_sha256=49f66b74f2cde9a33a2d1023b569d0b1a0a77571b06bd237662ca7393f4f4fa5 ;; \
      *) echo "unsupported target architecture: $TARGETARCH" >&2; exit 64 ;; \
    esac; \
    curl -sSL -o dolt.tgz "https://github.com/dolthub/dolt/releases/download/v1.86.4/$dolt_archive"; \
    echo "$dolt_sha256  dolt.tgz" | sha256sum -c -; \
    tar xzf dolt.tgz; \
    install -m 0755 "${dolt_archive%.tar.gz}/bin/dolt" /usr/local/bin/dolt; \
    rm -rf dolt.tgz "${dolt_archive%.tar.gz}"; \
    dolt config --global --add metrics.disabled true; \
    dolt config --global --add versioncheck.disabled true; \
    test "$(dolt config --global --get metrics.disabled)" = true; \
    test "$(dolt config --global --get versioncheck.disabled)" = true; \
    dolt version; \
    install -d /usr/local/share/dorg-suite-job; \
    { \
      printf 'DORG_SUITE_JOB_TARGETARCH=%q\n' "$TARGETARCH"; \
      printf 'DORG_SUITE_JOB_DOLT_ARCHIVE=%q\n' "$dolt_archive"; \
      printf 'DORG_SUITE_JOB_DOLT_SHA256=%q\n' "$dolt_sha256"; \
      printf 'DORG_SUITE_JOB_EMACS_VERSION=%q\n' "$EMACS_VERSION"; \
      printf 'DORG_SUITE_JOB_EMACS_SHA256=%q\n' "$EMACS_SHA256"; \
    } > /usr/local/share/dorg-suite-job/image.env

# Git maintenance's object-membership contract is verified on 2.49, not the
# Bookworm 2.39 package. Keep this after the existing toolchain build so the
# correction does not invalidate the expensive Emacs layer.
ARG GIT_VERSION=2.49.0
ARG GIT_SHA256=618190cf590b7e9f6c11f91f23b1d267cd98c3ab33b850416d8758f8b5a85628
RUN apt-get update && \
    apt-get install -y --no-install-recommends libcurl4-gnutls-dev libexpat1-dev libssl-dev gettext && \
    rm -rf /var/lib/apt/lists/*
RUN set -euo pipefail; \
    cd /tmp; \
    curl -fsSL -o git.tar.xz "https://www.kernel.org/pub/software/scm/git/git-${GIT_VERSION}.tar.xz"; \
    echo "${GIT_SHA256}  git.tar.xz" | sha256sum -c -; \
    tar xJf git.tar.xz; \
    cd "git-${GIT_VERSION}"; \
    make -j"$(nproc)" prefix=/usr/local NO_TCLTK=YesPlease all; \
    make prefix=/usr/local NO_TCLTK=YesPlease install; \
    cd /tmp; \
    rm -rf "git-${GIT_VERSION}" git.tar.xz; \
    test "$(git --version)" = "git version ${GIT_VERSION}"

# Packet admission uses only Python's standard library. Keep this layer after
# the pinned compiler/toolchain builds so it does not rebuild Emacs or Git.
RUN apt-get update && \
    apt-get install -y --no-install-recommends python3 && \
    rm -rf /var/lib/apt/lists/*
RUN apt-get update && \
    apt-get install -y --no-install-recommends tmux procps && \
    rm -rf /var/lib/apt/lists/*
COPY tools/dorg-suite-job-source.py /usr/local/bin/dorg-suite-job-source.py
COPY tools/dorg-suite-job-network.py /usr/local/bin/dorg-suite-job-network.py
COPY tools/dorg-suite-job-entrypoint /usr/local/bin/dorg-suite-job
RUN chmod 0755 /usr/local/bin/dorg-suite-job

ENTRYPOINT ["/usr/local/bin/dorg-suite-job"]
