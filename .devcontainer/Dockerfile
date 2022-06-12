# [Choice] alpine,...
ARG VARIANT="alpine"
FROM erlang:${VARIANT}

# ** [Optional] Uncomment this section to install additional packages. **
RUN apk update \
    && apk add --no-cache git make