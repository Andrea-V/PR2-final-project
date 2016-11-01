
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class Main {

	public static void main(String[] args) {
		String divcom1 = new String("Nel mezzo del cammin di nostra vita...");
		String divcom2 = new String("Voulsi così colà dove si puote...");
		String iliade1 = new String("Cantami, o Diva, del Pelìde Achille...");
		String donchi1 = new String("In un borgo della Mancia...");
		String ggmarq1 = new String("Molti anni dopo, di fronte al plotone...");
		
		Blog blog  =new Blog("supersegreta");
		Blogger bob=new Blogger("Bob");
		Blogger sam=new Blogger("Sam");
		Blogger tom=new Blogger("Tom");
		int id=0;
		
		blog.status();
		System.out.println(blog.repOk());
		// prova inserimento blogger
		try{
			blog.addBlogger(bob,"supersegreta");
			blog.addBlogger(sam,"supersegreta");
			blog.addBlogger(tom,"segreta");
		}catch(UnauthorizedAccessException exc){
			System.err.println("Errore nell'inserimento di "+exc.getMessage());
		}
		blog.status();
		System.out.println(blog.repOk());
		//prova eliminazione blogger
		try{
			blog.deleteBlogger(bob,"supersegreta");
		}catch(UnauthorizedAccessException exc){
			System.err.println("Errore nella rimozione di "+exc.getMessage());
		}
		blog.status();
		System.out.println(blog.repOk());
		//prova post messaggi
		try{
			blog.post(donchi1,sam);
			id=blog.post(iliade1,sam);
			blog.post(divcom1, bob);
			
		}catch(UnauthorizedBloggerException exc){
			System.err.println("Accesso non autorizzato di "+exc.getMessage());
			
		}
		blog.status();
		System.out.println(blog.repOk());
		//prova lettura messaggi
		try{
			blog.addBlogger(bob, "supersegreta");
			blog.post(divcom1,bob);
			System.out.println("\nUltimo post inserito: "+blog.readLast());
			System.out.println("Ultimo post inserito da Sam: "+blog.readLast(sam));
		}catch(EmptyPostException exc){
			System.err.println("Blog senza messaggi!");
		}catch(Exception exc){
			exc.printStackTrace();
			System.exit(1);
		}
		blog.status();
		System.out.println(blog.repOk());
		//prova lettura messaggi 2
		try{
			blog.addBlogger(tom, "supersegreta");
			blog.post(ggmarq1,tom);
			blog.post(divcom2,bob);
			
			List<String>  bobpost=blog.readAll(bob);
			List<String>  allpost=blog.readAll();
			
			System.out.println("\nTutti i post di Bob:");
			for(String msg:bobpost)
				System.out.println("\t"+msg);
			
			System.out.println("\nTutti i post");
			for(String msg:allpost)
				System.out.println("\t"+msg);
			
		}catch(Exception exc){
			exc.printStackTrace();
			System.exit(1);
		}
		blog.status();
		System.out.println(blog.repOk());
		//prova eliminazione messaggio
		try{
			blog.delete(id);
			blog.status();
			blog.delete(42);
		}catch(WrongCodePostException exc){
			System.err.println("Messaggio non presente: "+exc.getMessage());
			
		}
		
		//prova blog vuoto
		try{
			blog.delete(0);
			blog.delete(2);
			blog.delete(3);
			blog.delete(4);
		}catch(WrongCodePostException exc){
			System.err.println("Messaggio non presente: "+exc.getMessage());
			
		}
		System.out.println(blog.emptyBlog());
		blog.status();
		System.out.println(blog.repOk());
	}
	
}

