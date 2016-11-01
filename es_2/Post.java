
public class Post {
	
	private int id;
	private String author;
	private String text;
	
	public Post(int id,String aut,String txt){
		this.id=id;
		this.author=aut;
		this.text=txt;
	}

	public String getAuthor() {
		return new String(author);
	}

	public int getId() {
		return id;
	}

	public String getText() {
		return new String(text);
	}
	
	public String toString(){
		return this.author+":"+this.text;
	}
}
