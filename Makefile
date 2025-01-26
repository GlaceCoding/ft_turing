NAME=env_turing

all: $(NAME)

re: fclean
	make all

$(NAME):
	docker build -t $(NAME) .
	docker run -it $(NAME)

clean:
	echo "Nothing to rm"

fclean: clean

clean_docker:
	docker rm -vf $(docker ps -aq)
	docker rmi -f $(docker images -aq)

.PHONY: all re $(NAME) clean fclean clean_docker