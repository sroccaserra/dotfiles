# Defined in - @ line 0
function goups --description 'alias goups=git add -p ; and git commit --amend --no-edit ; and git push --force-with-lease'
	git add --patch ; and git commit --amend --no-edit ; and git push --force-with-lease $argv;
end
