FROM swipl:stable

# Install jq for json querying in bash
RUN ["apt-get", "update"]
RUN ["apt-get", "-y", "install", "python3"]

# Make sure the students can't find our secret path, which is mounted in
# /mnt with a secure random name.
RUN ["chmod", "711", "/mnt"]

# Add the user which will run the student's code and the judge.
RUN ["useradd", "-m", "runner"]

# As the runner user
WORKDIR /home/runner
USER runner

    # Install the more packages


    # Create the working directory
    RUN ["mkdir", "workdir"]

USER root

WORKDIR /home/runner/workdir
ENTRYPOINT [ "bash" ]
COPY main.sh /main.sh
