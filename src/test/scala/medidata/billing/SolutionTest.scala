package medidata.billing

import medidata.billing.Solution._
import org.scalacheck.Gen
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks

import scala.math.BigDecimal.RoundingMode

class SolutionTest extends WordSpec with Matchers with PropertyChecks {

  "totalCost with no discount for all age groups and insurance" should {
    /*    val servicesGen = Gen.listOf(Gen.oneOf(Diagnosis, XRay, BloodTest, ECG ))*/
    val services = List(Diagnosis, XRay, BloodTest, ECG, Vaccine, Vaccine)
    val discountFun: Discount = (_, _, _, _) => 1d
    val ageGen = Gen.choose(0, 200)
    val insuranceGen = Gen.oneOf(true, false)

    "produce a sum of nominal cost of the services" in {
      forAll(insuranceGen, ageGen) { (insurance: Insurance, age) => {
        val cost = totalCost(services, insurance, age, discountFun)
        val expected = BigDecimal(60d + 150d + 78d + 200.40d + 27.50d + 15d + 15d).setScale(2, RoundingMode.HALF_EVEN)
        assert(cost == 60d + 150d + 78d + 200.40d + 27.50d + 15d + 15d)
      }
      }
    }
  }
  "totalCost with standard discount and no insurance" should {
    val services = List(Diagnosis, XRay, BloodTest, ECG, Vaccine, Vaccine)
    val ageGen = Gen.choose(0, 200)
    /*    val insuranceGen = Gen.oneOf(true, false)*/
    "produce a sum of nominal cost of the services discounted by age group" in {
      forAll(ageGen) { (age) => {
        whenever(age > 0) {
          val cost = totalCost(services, false, age)
          val ageDiscount =
            age match {
              case a if a >= 0 && a < 5 => 0.4d
              case a if a >= 5 && a < 65 => 0d
              case a if a >= 65 && a < 70 => 0.6d
              case a if a >= 70 => 0.9d
            }

          val expected = (60d + 150d + 78d + 200.40d + 27.50d + 15d + 15d) * (1d - ageDiscount)
          assert(cost == BigDecimal(expected).setScale(2, RoundingMode.HALF_EVEN))
        }
      }
      }
    }
  }
  "BloodTest if goes with insurance and diagnosed in MediHealth " should {
    val testWithDiagnosis = List(Diagnosis, BloodTest)
    val testWithNoDiagnosis = List(BloodTest)
    val ageGen = Gen.choose(0, 200)
    " give additional 15% discount" in {
      forAll(ageGen) { (age) => {
        whenever(age > 0) {
          val costWith = totalCost(testWithDiagnosis, true, age)
          val costWithout = totalCost(testWithNoDiagnosis, true, age)
          val ageDiscount =
            age match {
              case a if a >= 0 && a < 5 => 0.4d
              case a if a >= 5 && a < 65 => 0d
              case a if a >= 65 && a < 70 => 0.6d
              case a if a >= 70 => 0.9d
            }

          val expectedWith = (60d + 78d * (1d - 0.15d)) * (1d - ageDiscount)
          val expectedWithout = (78d) * (1d - ageDiscount)
          assert(costWith == BigDecimal(expectedWith).setScale(2, RoundingMode.HALF_EVEN))
          assert(costWithout == BigDecimal(expectedWithout).setScale(2, RoundingMode.HALF_EVEN))

        }
      }
      }
    }
  }
}
