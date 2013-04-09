import collection.JavaConversions.asScalaIterator
import java.io.File
import org.apache.commons.io.FileUtils
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.index.IndexManager
import org.neo4j.graphdb._
import sys.ShutdownHookThread

object Types {
    type Version = String
}
import Types._

trait AmpComponent {
    val name: String
    val version:Version
    val unittype: String

    override def toString:String = "%s-%s,%s".format(name,version,unittype)
}

object AmpComponent {
    val PK="name"
    val VERSION = "version"
}

case class AmpComponentWar(name: String,version:Version) extends AmpComponent {
    override val unittype = "WAR"
}

case class AmpComponentSCA(name: String,version:Version) extends AmpComponent {
    override val unittype = "SCA"
}

object RelTypes extends Enumeration {
    type RelTypes = Value
    val DEPENDS,
        VERSION = Value

    implicit def conv(rt: RelTypes) = new RelationshipType() {def name = rt.toString}
}

object Artificer extends App {

    def getVersions(n:Node):Seq[Version] = {
        val rels = n.getRelationships(RelTypes.VERSION)

        rels.iterator.map( r => r.getEndNode.getProperty(AmpComponent.VERSION).asInstanceOf[Version]).toList
    }
    def getDeps(n:Node):Seq[AmpComponent] = {
        val rels = n.getRelationships(RelTypes.VERSION)

        val deps =
        for( r <- rels.iterator; dep = if(null==r.getEndNode.getRelationships(RelTypes.DEPENDS)) None else
                                          r.getEndNode.getRelationships(RelTypes.DEPENDS).iterator().next().getEndNode)
            yield dep

        deps.filter(_!=None).map( d => new AmpComponent{
            val name = ""
            val version = ""
            val unittype = ""
        }).toList
    }

    def getComponentByVersion(n:Node):Seq[String] = {
        val rels = n.getRelationships(RelTypes.VERSION,Direction.INCOMING)

        rels.iterator.map( r => r.getStartNode.getProperty(AmpComponent.PK).asInstanceOf[String]).toList
    }

    def parseComponent(c:String) = if(c.isEmpty) ("","") else (c.split(":")(0),c.split(":")(1))

    val neo4jStoreDir = "/tmp/tmp-neo-artificer"

    // clear
    FileUtils.deleteDirectory(new File(neo4jStoreDir));

    val graphDb : GraphDatabaseService  = new GraphDatabaseFactory().
        newEmbeddedDatabaseBuilder( neo4jStoreDir ).
        //loadPropertiesFromFile( "neo4j.properties" ).
        newGraphDatabase();
    val indexName = graphDb.index().forNodes( "names" );
    val indexVersion = graphDb.index().forNodes( "versions" );
    val indexVersioning = graphDb.index().forRelationships( "versioning" );


    Runtime.getRuntime().addShutdownHook(new Thread() {
        override def run() {
            graphDb.shutdown();
        }
    });


    /**
     * defining nodes
     */
    final val assembly = Map(
        "Scheduler:1.0.0" -> "",
        "Scheduler:2.0.0" -> "",
        "Scheduler:4.0.0" -> "",
        "Translation:2.0.0" -> "",
        "FolderProcessor:1.0.0" -> "Scheduler:1.0.0",
        "FolderProcessor:2.0.0" -> "Scheduler:2.0.0",
        "App:3.0.0" -> "Scheduler:1.0.0,Translation:2.0.0"
    )


    /**
     * creating nodes and associations
     */
    val tx = graphDb.beginTx()
    try {
//            val pre1 =
//                for ((c:String,deps) <- assembly;
//                    name = c.split(":")(0);
//                    version = c.split(":")(0);
//                    component = AmpComponentWar(name,version)
//                ) yield (createNode(component),component, deps)
//
//            val db =
//                for( (node,component,deps) <- pre1;
//
//                ) yield (component.name, node)

        for ((c: String, deps) <- assembly;
             (name,version) = parseComponent(c);
             component = AmpComponentWar(name, version)
        ) {
            println("Creating: [%s:%s]".format(component,version))
            // find node to avoid duplicates


            val n1 = indexName.get(AmpComponent.PK,name).getSingle() match {
                case null => {
                    val n = graphDb.createNode()
                    n.setProperty(AmpComponent.PK,component.name)
                    indexName.add( n, AmpComponent.PK, component.name );
                    n
                }
                case _ @ n => n
            }

            //  version
            val n2 = graphDb.createNode()
            n2.setProperty(AmpComponent.VERSION,component.version)
            indexVersion.add( n2, AmpComponent.VERSION, component.version );

            val rv = n1.createRelationshipTo(n2,RelTypes.VERSION)
            indexVersioning.add(rv,"name",component.name)
            indexVersioning.add(rv,"version",component.version)

            // dependancy
            for ( dep <- deps.split(",") ) {
                val (name,version) = parseComponent(dep)
                if (!version.isEmpty) {
                    println("[%s]: building dependancy: %s:%s".format(component, name, version))
                    // find component/version
                    val r = indexVersioning.query("name:%s AND version:%s".format(name, version)).getSingle
                    println("[%s]: dependancy: %s".format(component, r))
                    if (null != r) {
                        // add relationship
                        val dn = r.getEndNode
                        println("[%s]: --> %s".format(component, dn))
                        n2.createRelationshipTo(dn, RelTypes.DEPENDS)
                    }
                }
            }

        }



        tx.success
    } catch {
        case e:Exception => { e.printStackTrace }
        tx.failure
    } finally {
        tx.finish
    }

    val hits = graphDb.index().forNodes(AmpComponent.PK).query("*:*");
    println("hits: %s".format(hits))
    try {
        while (hits.hasNext()) {
            val n = hits.next();
            println("node: %s: '%s'".format(n,n.getProperty(AmpComponent.PK)))
        }
    } finally {
        hits.close();
    }

    {
        val found = indexName.get(AmpComponent.PK, "Scheduler")
        found.foreach(n => println("%s: '%s: %s'".format(n, n.getProperty(AmpComponent.PK), getVersions(n))))
    }

    {
        val found = indexVersion.get(AmpComponent.VERSION, "2.0.0")
        found.foreach(n => println("%s: '%s: %s'".format(n, n.getProperty(AmpComponent.VERSION), getComponentByVersion(n))))
    }


    //println("Found: %s".format(found))

    graphDb.shutdown()
}
