from glob import glob
import os
import os
import shutil
import stat
import sys

# thanks to http://stackoverflow.com/questions/1868714
def copytree(src, dst, symlinks = False, ignore = None):
  if not os.path.exists(dst):
    os.makedirs(dst)
    shutil.copystat(src, dst)
  lst = os.listdir(src)
  if ignore:
    excl = ignore(src, lst)
    lst = [x for x in lst if x not in excl]
  for item in lst:
    s = os.path.join(src, item)
    d = os.path.join(dst, item)
    if symlinks and os.path.islink(s):
      if os.path.lexists(d):
        os.remove(d)
      os.symlink(os.readlink(s), d)
      try:
        st = os.lstat(s)
        mode = stat.S_IMODE(st.st_mode)
        os.lchmod(d, mode)
      except:
        pass # lchmod not available
    elif os.path.isdir(s):
      copytree(s, d, symlinks, ignore)
    else:
      shutil.copy2(s, d)

def cp_r(frm, to):
    src = frm.replace('/','\\')
    dst = to.replace('/','\\')
    if os.path.isdir(src):
        copytree(src, dst)
    else:
        for fname in glob(src):
            shutil.copy(fname, dst)

if __name__ == "__main__":
    if len(sys.argv) >= 2:
        cp_r(sys.argv[1], sys.argv[2])
