package medidata.billing


import scala.math.BigDecimal.RoundingMode

object Solution {

  sealed trait MedidataService

  case object Diagnosis extends MedidataService

  case object XRay extends MedidataService

  case object BloodTest extends MedidataService

  case object ECG extends MedidataService

  case object Vaccine extends MedidataService

  type Insurance = Boolean
  type MedidataDiagnosed = Boolean
  type Age = Int
  type Discount = (MedidataDiagnosed, Insurance, Age, MedidataService) => Double

  def totalCost(services: List[MedidataService], insurance: Insurance, age: Age, discount: Discount = getDiscount): BigDecimal = {
    val wasDiagnosed: MedidataDiagnosed = services.contains(Diagnosis)
    val resDec = services.groupBy(identity).mapValues(_.size)
      .map {
        case (service, count) => {
          service match {
            case Diagnosis => 60d * count
            case XRay => 150d * count
            case BloodTest => count * 78d
            case ECG => 200.40d * count
            case Vaccine => 27.50d + 15d * count
          }
        } * discount(wasDiagnosed, insurance, age, service)
      }.foldLeft(0d) { case (acc, c) => acc + c }
    BigDecimal(resDec).setScale(2, RoundingMode.HALF_EVEN)
  }


  def getDiscount: Discount = {
    case (wasDiagnosed, insurance, age, service) =>
      val serviceDiscount = service match {
        case BloodTest => if (insurance && wasDiagnosed) 0.15d else 0d
        case _ => 0d
      }
      val ageDiscount =
        age match {
          case a if a >= 0 && a < 5 => 0.4d
          case a if a >= 5 && a < 65 => 0d
          case a if a >= 65 && a < 70 => 0.6d
          case a if a >= 70 => 0.9d
          case _ => throw new IllegalArgumentException
        }
      (1d - serviceDiscount) * (1d - ageDiscount)
  }
}
