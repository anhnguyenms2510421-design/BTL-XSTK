Cài Git và đảm bảo đã có quyền truy cập repository GitHub, mở CMD trên Windows, copy-paste code từ dòng 3 → 7:

cd /d [Thư mục muốn chứa file] (VD: cd /d D:\VScode\LaTeX )
git clone https://github.com/anhnguyen2510421-design/BTL-XSTK.git
cd LaTeX_XSTK
git switch -c [Tên branch] (VD: git switch -c new_branch )
code .

Sau đó chỉnh sửa code. Khi chỉnh sửa xong, copy-paste:

git add .
git commit -m "[Tên commit]" (VD: git commit -m "Update Chapter 2" )
git push -u origin [Tên branch] (VD: git push -u origin new_branch)

Sau đó vào Github:

Pull requests
→ New pull request
→ base: main
→ compare: [Tên branch] (VD: compare: new_branch )
→ Create pull request

Chờ được approve và merge vào main.
