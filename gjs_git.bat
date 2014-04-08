@REM assumes git is in PATH
git config --global user.email "phonixor@gmail.com"
git config --global user.name "phonixor"
REM and now for the project
touch README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/phonixor/microplate.git
git push -u origin master

pause

