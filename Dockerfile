FROM microsoft/dotnet:sdk

# add a non-root user and give them ownership
RUN useradd -u 9000 app && \
  # user home directory
  mkdir /home/app && \
  chown -R app:app /home/app && \
  # repo
  mkdir /repo && \
  chown -R app:app /repo && \
  # collector code
  mkdir -p /usr/src/collector && \
  chown -R app:app /usr/src/collector

WORKDIR /usr/src/collector
COPY src/ /usr/src/collector/

# Restore & build
RUN dotnet restore && \
  dotnet publish -c Release -o /usr/src/collector/bin && \
  chown -R app:app /usr/src/collector/bin && \
  ls -la /usr/src/collector/bin && \
  dotnet --info

# run everything from here on as non-root
USER app

# Set workdir to /repo
WORKDIR /repo

ENTRYPOINT ["dotnet", "/usr/src/collector/bin/YoloDev.DependenciesIO.Nuget.Collector.dll"]
CMD ["/"]