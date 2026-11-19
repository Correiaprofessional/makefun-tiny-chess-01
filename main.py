import os,urllib.request,json
P,N,B,R,Q,K=1,2,3,4,5,6
ND=[(1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1),(-2,1),(-1,2)]
RD=[(1,0),(-1,0),(0,1),(0,-1)]
BD=[(1,1),(1,-1),(-1,1),(-1,-1)]
QD=RD+BD
V={P:100,N:320,B:330,R:500,Q:900,K:0,0:0}
T=[[(ord(c)-75)*5 for c in s]for s in('KKKKKKKKLMMGGMMLLJIKKIJLKKKOOKKKLLMPPMLLMMOQQOMMUUUUUUUUKKKKKKKK','ACEEEECACGKLLKGCELMNNMLEEKNOONKEELNOONLEEKMNNMKECGKKKKGCACEEEECA','GIIIIIIGILKKKKLIIMMMMMMIIKMMMMKIILLMMLLIIKLMMLKIIKKKKKKIGIIIIIIG','KKKLLKKKJKKKKKKJJKKKKKKJJKKKKKKJJKKKKKKJJKKKKKKJLMMMMMMLKKKKKKKK','K'*64,'OQMKKMQOOOKKKKOOIGGGGGGIGEECCEEGECCAACCEECCAACCEECCAACCEECCAACCE')]
T+=[T[4]]
def bf(s):
 b=[0]*64;y=7;M='PNBRQK'
 for r in s.split('/'):
  x=0
  for c in r:
   if c>'9':b[y*8+x]=(1 if c.isupper() else -1)*(M.index(c.upper())+1);x+=1
   else:x+=int(c)
  y-=1
 return b
def um(s,b):
 f=(ord(s[0])-97)+(int(s[1])-1)*8;t=(ord(s[2])-97)+(int(s[3])-1)*8;p=b[f]
 q={'q':Q,'r':R,'b':B,'n':N}.get(s[4:5])
 if q:return(f,t,q)
 if abs(p)==K and abs(t-f)==2:return(f,t,7)
 if not b[t] and abs(p)==P and t%8!=f%8:return(f,t,8)
 return(f,t,0)
def gm(b,c):
 ms=[]
 for f in range(64):
  p=b[f]
  if not p or (p>0)!=(c>0):continue
  t=abs(p);r=f//8;x=f%8
  if t==P:
   d=8 if p>0 else -8
   if b[f+d]==0:
    if (f+d)//8 in(0,7):ms+=[(f,f+d,Q),(f,f+d,N),(f,f+d,R),(f,f+d,B)]
    else:
     ms.append((f,f+d,0))
     if (r==1 if p>0 else r==6) and b[f+2*d]==0:ms.append((f,f+2*d,0))
   for dx in-1,1:
    if 0<=x+dx<8:
     t2=f+d+dx
     if b[t2]*p<0:ms+= [(f,t2,Q),(f,t2,N),(f,t2,R),(f,t2,B)] if t2//8 in(0,7) else [(f,t2,0)]
  elif t in(2,6):
   for dr,dx in(ND if t==N else QD):
    r2=r+dr;x2=x+dx
    if 0<=r2<8 and 0<=x2<8:
     t2=r2*8+x2
     if b[t2]*p<=0:ms.append((f,t2,0))
  else:
   for dr,dx in(QD if t==Q else BD if t==B else RD):
    r2=r+dr;x2=x+dx
    while 0<=r2<8 and 0<=x2<8:
     t2=r2*8+x2;q=b[t2]
     if q*p>0:break
     ms.append((f,t2,0))
     if q:break
     r2+=dr;x2+=dx
 return ms
def ik(b,c):
 for f in range(64):
  p=b[f]
  if p*c>0 and abs(p)==K:break
 else:return False
 r,x=divmod(f,8)
 for dx in-1,1:
  r2=r+c;x2=x+dx
  if 0<=r2<8 and 0<=x2<8 and b[r2*8+x2]==-c*P:return True
 for dr,dx in ND:
  r2=r+dr;x2=x+dx
  if 0<=r2<8 and 0<=x2<8 and b[r2*8+x2]==-c*N:return True
 for dr,dx in QD:
  r2=r+dr;x2=x+dx;n=0
  while 0<=r2<8 and 0<=x2<8:
   q=b[r2*8+x2];n+=1
   if q:
    if q*c<0:
     t=abs(q)
     if t==Q or t==K and n==1 or t==R and not(dr and dx) or t==B and dr and dx:return True
    break
   r2+=dr;x2+=dx
 return False
def mv(b,f,t,pc=0):
 b=b[:];p=b[f];k=f+3 if t>f else f-4
 if pc==8:b[t-(8 if p>0 else -8)]=0
 elif pc==7:b[t-1 if t>f else t+1]=b[k];b[k]=0
 elif pc:p=pc if p>0 else -pc
 b[t]=p;b[f]=0;return b
def ev(b,c):
 s=0
 for i,p in enumerate(b):
  if p:s+=p//abs(p)*(V[abs(p)]+T[abs(p)-1][i if p>0 else i^56])
 return s if c>0 else -s
C=[0]
def ss(b,c,d,a=-99999,bv=99999):
 C[0]+=1
 ms=gm(b,c);q=d<1
 if q:ms=[m for m in ms if b[m[1]]]
 vv=ev(b,c)if q else -99999
 if vv>a:a=vv
 bm=None
 ms.sort(key=lambda m:-V[abs(b[m[1]])]+V[abs(b[m[0]])])
 for m in ms:
  nb2=mv(b,*m)
  if ik(nb2,c):continue
  x=-ss(nb2,-c,d-1,-bv,-a)[0]
  if x>vv:vv=x;bm=m
  if x>a:a=x
  if a>=bv or C[0]>2e4:break
 if not bm:
  for m in gm(b,c):
   if not ik(mv(b,*m),c):return vv,None
  return(-90000 if ik(b,c) else 0),None
 return vv,bm
def cm(b,c,ls):
 r=[(s,um(s,b)) for s in ls]
 best=None
 for d in(1,2,3):
  C[0]=0;vv=-99999;bm=None
  for s,m in r:
   x=-ss(mv(b,*m),-c,d)[0]
   if x>vv:vv=x;bm=s or m
  if C[0]>2e4:break
  best=bm
 return best or(r[0][0]if r else 0)
def p(u,d):
 q=urllib.request.Request(u,data=json.dumps(d).encode(),headers={'Content-Type':'application/json'})
 return json.loads(urllib.request.urlopen(q,timeout=8).read())
def run():
 e=os.environ['MAKEFUN_ENV_URL']
 while 1:
  g=p(e+'/game',{})
  if g['status']=='done':return
  while g['status']=='your_move':
   t=g['fen'].split();m=cm(bf(t[0]),1 if t[1]=='w'else -1,g['legal_moves'])
   if not m:return
   try:g=p(e+'/move',{'game':g['game'],'move':m})
   except:g={}
if __name__=='__main__':run()
