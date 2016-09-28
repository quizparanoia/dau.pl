VERSION := "2.5.0"

REPO := dau.pl
USER := quizparanoia
TOKEN = `cat .token`

OBJS  = dau.pl.tgz DAU.pm.tgz

%.tgz:
	@tar -czf $@ $(subst .tgz, ,$@)

release-create:
	@git tag $(VERSION) -f && git push --tags -f
	@github-release release --user $(USER) --repo $(REPO) --tag $(VERSION) -s $(TOKEN)

release: release-create $(OBJS)
	@for artifact in $(filter-out release-create,$^); do \
			github-release upload --user $(USER) --repo $(REPO) --tag $(VERSION) -s $(TOKEN) --name $$artifact --file $$artifact; \
		done

retract:
	@github-release delete --tag $(VERSION) -s $(TOKEN)

clean:
	@rm *.tgz

.PHONY: release-create release retract clean
