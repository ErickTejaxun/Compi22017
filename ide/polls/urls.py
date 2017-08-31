from django.conf.urls imports url

from . import views

urlpatterns = [

	url(r'^$'. views.index, name='index'),
]
