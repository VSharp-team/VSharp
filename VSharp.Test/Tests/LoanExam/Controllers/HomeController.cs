using LoanExam.Models;
using Microsoft.AspNetCore.Mvc;

namespace LoanExam.Controllers;

public class HomeController: Controller
{
    [HttpGet]
    public ActionResult Index()
    {
        return View(new Request(
            new Personality(),
            new CreditInfo()
            {
                Sum = 100500
            },
            new Passport()
            {
                Number = "123"
            },
            true,
            true
        ));
    }

    [HttpPost]
    public ActionResult Index2([FromForm] Request request)
    {
        return Ok("345");
    }
}