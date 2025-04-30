# OpenLDAP y phpLDAPadmin con Docker

Este proyecto contiene la configuración necesaria para ejecutar un servidor OpenLDAP junto con la interfaz de administración phpLDAPadmin utilizando Docker.

## Componentes

- **OpenLDAP**: Servidor LDAP para almacenar y gestionar directorios de usuarios y organizaciones.
- **phpLDAPadmin**: Interfaz web para administrar el servidor OpenLDAP.

## Requisitos previos

- Docker
- Docker Compose

## Configuración

Antes de iniciar los servicios, puedes modificar la configuración en los siguientes archivos:

- `docker-compose.yml`: Modifica las variables de entorno para ajustar el dominio, organización y contraseñas.
- `config.php`: Ajusta la configuración de phpLDAPadmin si es necesario.

## Uso

### Iniciar los servicios

```bash
cd ldap-docker
docker-compose up -d
```

### Acceder a phpLDAPadmin

Una vez que los servicios estén en funcionamiento, puedes acceder a phpLDAPadmin a través de tu navegador:

```
http://localhost:8080
```

Credenciales de inicio de sesión:

- Login DN: `cn=admin,dc=midominio,dc=local`
- Contraseña: `admin_password` (o la que hayas configurado en `docker-compose.yml`)

### Detener los servicios

```bash
docker-compose down
```

### Eliminar los volúmenes (datos persistentes)

```bash
docker-compose down -v
```

## Personalización

### Cambiar el dominio y organización

Modifica las siguientes variables en `docker-compose.yml`:

```yaml
LDAP_ORGANISATION: "Tu Organización"
LDAP_DOMAIN: "tudominio.com"
```

Recuerda también actualizar la configuración en `config.php` para que coincida con tu dominio:

```php
$servers->setValue('server','base',array('dc=tudominio,dc=com'));
$servers->setValue('login','bind_id','cn=admin,dc=tudominio,dc=com');
```

## Seguridad

Por defecto, esta configuración utiliza contraseñas simples. En un entorno de producción, deberías:

1. Cambiar todas las contraseñas por unas seguras
2. Configurar SSL/TLS para conexiones seguras
3. Restringir el acceso a los puertos mediante un firewall
