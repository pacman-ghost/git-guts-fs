PROJECTS=git-guts cli git-guts/tests

build:
	dotnet build --nologo --no-restore cli/cli.fsproj
build-tests:
	dotnet build --nologo --no-restore git-guts/tests/tests.fsproj

publish:
	for proj in $(PROJECTS); do \
		rm -rf $$proj/bin/Release ; \
		rm -rf $$proj/obj/Release ; \
	done
	dotnet publish --nologo --no-restore --configuration Release cli/cli.fsproj

tests:
	dotnet test --nologo --no-restore git-guts/tests/

clean:
	for proj in $(PROJECTS); do \
		rm -rf $$proj/bin ; \
		rm -rf $$proj/obj ; \
	done

restore:
	for proj in $(PROJECTS); do \
		dotnet restore $$proj/*.fsproj ; \
	done
