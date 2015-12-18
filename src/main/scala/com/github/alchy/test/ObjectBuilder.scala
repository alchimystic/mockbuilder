package com.github.alchy.test

import java.lang.reflect.{Constructor, Modifier}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

/**
 * Trait for creating mocked objects, especially convenient to be used with in tests and case classes with lots of fields.
 * The copy() method of case class should be used then to complete the object as we need.
 *
 * Objects will be as simple as possible:
 * - numeric values: 1
 * - boolean: false
 * - String: dummy
 * - collections: Nil
 * - Set: empty
 * - Map: empty
 * - Option: None
 *
 * Enumerations will never be correct (because of type erasure), so when needed, we have to set the proper value.
 *
 * Created by carlos on 14/12/15.
 */
trait ObjectBuilder {

  val classes: ListBuffer[Class[_]] = ListBuffer()

  val valueMap: Map[Class[_], Object] = Map(
    classOf[Byte] -> Byte.box(1),
    classOf[Short] -> Short.box(1),
    classOf[Int] -> Int.box(1),
    classOf[Long] -> Long.box(1),
    classOf[Double] -> Double.box(1),
    classOf[Boolean] -> Boolean.box(false),
    classOf[String] -> "dummy", classOf[Option[_]] -> None,
    classOf[Enumeration#Value] -> DummyEnum.dummy,
    classOf[Set[_]] -> Set.empty,
    classOf[Map[_, _]] -> Map.empty,
    classOf[List[_]] -> Nil,
    classOf[Seq[_]] -> Nil)

  def using(objects: Object*): ObjectCache = {
    val cache = new ObjectCache
    cache.classes.appendAll(this.classes)
    cache.using(objects: _*)
    cache
  }

  def addClass[T](implicit tag: ClassTag[T]): ObjectBuilder = {
    classes.append(tag.erasure)
    this
  }

  protected def getImpl[T](cl: Class[T]): T = {
    classes.find(cl.isAssignableFrom(_)) match {
      case Some(impl) ⇒ return internalCreate(impl).asInstanceOf[T]
      case None       ⇒ throw new UnsupportedOperationException("dont know how to instantiate " + cl)
    }
  }

  def create[T](implicit tag: ClassTag[T]): T = internalCreate(tag.runtimeClass).asInstanceOf[T]

  def hasCustomImpl(cl: Class[_]): Boolean = false

  private def internalCreate[T](cl: Class[T]): T = {
    if (cl.isInterface || Modifier.isAbstract(cl.getModifiers) || hasCustomImpl(cl)) {
      return getImpl(cl)
    }

    val cons = getBestConstructor(cl)
    createInstance(cons)
  }

  def getBestConstructor[T](cl: Class[T]): Constructor[T] = {
    val all = cl.getConstructors
    if (all.isEmpty) {
      throw new UnsupportedOperationException("No constructors for " + cl)
    }
    all(0).asInstanceOf[Constructor[T]]
  }

  def createInstance[T](cons: Constructor[T]): T = {
    val values = cons.getParameterTypes.map { tp ⇒
      valueMap.get(tp) match {
        case Some(value) ⇒ value
        case None        ⇒ internalCreate(tp).asInstanceOf[Object]
      }
    }
    try {
      cons.newInstance(values: _*)
    } catch {
      case ex: Exception ⇒
        //println(ex.getCause.toString)
        throw ex
    }
  }

}

/**
 * Specialization of ObjectBuilder that can keep implementations to be picked for future mock creations.
 * Whenever a field has an abstract type, we will look for implementations in a list,
 * and the 1st suitable will be picked.
 */
class ObjectCache extends ObjectBuilder {

  val implementations: ListBuffer[Object] = ListBuffer()

  override def using(objects: Object*): ObjectCache = {
    implementations.append(objects: _*)
    this
  }

  override protected def getImpl[T](cl: Class[T]): T = implementations.find(cl.isInstance(_)) match {
    case Some(impl) ⇒ impl.asInstanceOf[T]
    case None       ⇒ super.getImpl(cl)
  }

  override def hasCustomImpl(cl: Class[_]): Boolean = implementations.exists(cl.isInstance(_))
}


object DummyEnum extends Enumeration {
  val dummy = Value
}
