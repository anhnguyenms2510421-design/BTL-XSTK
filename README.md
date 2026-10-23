Cách lấy folder:

B1: mở CMD, vào folder cần lữu trữ.

B2:
git clone https://github.com/anhnguyenms2510421-design/BTL-XSTK
cd BTL-XSTK
git switch -c chapter1
git branch
code .

B3: 
chỉnh sửa phần làm của mình.
Lưu ý: chỉ chỉnh sửa phần Chapter, Image, Package của chapter tương ứng, không chỉnh sửa code trong main

B4: 
git status
git add .
git status
git commit -m "Update chapter x"

quan sát sự thay đổi trước khi push code

B5:
git push -u origin chapter1
