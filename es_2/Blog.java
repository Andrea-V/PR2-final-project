import java.util.ArrayList;
import java.util.List;

public class Blog implements SimpleBlog {
	
	private final String password;
	private ArrayList<Blogger> abloggers;
	private ArrayList<Post> posts;
	private int idpost;
	
	public Blog(String pwd){
		this.password=(pwd!=null)?(pwd):("");
		this.idpost=0;
		this.abloggers=new ArrayList<Blogger>();
		this.posts=new ArrayList<Post>();
	}
	
	public void status(){
		System.out.println(this.toString());
	}
	
	public void addBlogger(Blogger bob, String pass) throws UnauthorizedAccessException{
			if(pass.equals(this.password))
				abloggers.add(bob);
			else
				throw new UnauthorizedAccessException(bob.getNickname());
	}

	public void deleteBlogger(Blogger bob, String pass) throws UnauthorizedAccessException {
		if(pass.equals(this.password))
			abloggers.remove(bob);
		else
			throw new UnauthorizedAccessException(bob.getNickname());

	}

	public int post(String message, Blogger bob) throws UnauthorizedBloggerException {
		if(abloggers.contains(bob)){
			posts.add(new Post(idpost,bob.getNickname(),message));
			idpost++;
		}
		else 
			throw new UnauthorizedBloggerException(bob.getNickname());
		return idpost-1;
	}
	
	public String readLast(Blogger bob) throws EmptyPostException {
		int size=posts.size();
		for(int i=size-1;i>=0;i--){
			Post p=posts.get(i);
			if(bob.getNickname().equals(p.getAuthor()))
				return p.toString();
		}
		throw new EmptyPostException();
	}

	public String readLast() throws EmptyPostException {
		if(this.emptyBlog())
			throw new EmptyPostException();
		else
			return posts.get(posts.size()-1).toString();
	}

	public List<String> readAll(Blogger bob) {
		ArrayList<String> result=new ArrayList<String>();
		
		for(Post p:posts)
			if(bob.getNickname().equals(p.getAuthor()))
				result.add(p.toString());
		
		return result;
	}

	public List<String> readAll() {
		ArrayList<String> result=new ArrayList<String>();
		
		for(Post p:posts)
			result.add(p.toString());
		
		return result;
	}

	public void delete(int code) throws WrongCodePostException {
		Post tmp=null;
		
		for(Post p:posts)
			if(p.getId()==code)
				tmp=p;
		if(tmp!=null)
			posts.remove(tmp);
		else
			throw new WrongCodePostException(Integer.toString(code));
	}

	public boolean emptyBlog() {
		return posts.size()==0;
	}
	
	public String toString(){
		String str=new String("\n+==== STATUS ====\n");
		
		str+="|- Active bloggers: "+abloggers.size()+"\n";
		for(Blogger b:abloggers)
			str+="|\t"+b.getNickname()+"\n";
		
		str+="|- Messages:        "+posts.size()+"\n";
		for(Post p:posts)
			str+="|\t"+p.getId()+":"+p.toString()+"\n";
		str+="+=============\n";
		return str;
	}

	public boolean repOk(){
		boolean p1=this.password!=null && this.abloggers!=null && this.posts != null;
		boolean p2=this.abloggers.size()>=0 && this.posts.size()>=0;
		boolean p3=this.idpost>=this.posts.size();
		
		return p1 && p2 && p3;
	}
}
